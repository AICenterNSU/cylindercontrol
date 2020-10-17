from typing import TypeVar
import gym
from gym import spaces
from gym.utils import seeding
import numpy as np
from collections import deque
import os.path
import time
import subprocess
import os
import signal
import math
import random

from matplotlib import pyplot
from matplotlib.animation import FuncAnimation

float_or_int = TypeVar('float_or_int', int, float)

class VirtualEnv(gym.Env):
    metadata = {
        'render.modes': ['human', 'rgb_array'],
        'video.frames_per_second': 240
    }

    def __init__(self, rank=0):

        if rank==0:
            self.commonfolder = '../'
            self.folder = self.commonfolder
        else:
            self.commonfolder = '../../'
            self.folder = self.commonfolder + str(rank) + '/'

        self.rank = rank

        self.path_config = self.folder + 'OpenAIcylinder.cfg'#folder + 'general.cfg'

        with open(self.path_config) as f:
            all_lines = f.read()
            lines = all_lines.split('\n')
            lines_without_comments = []

            for l in lines:
                if len(l) == 0:
                    continue
                if l[0] == '#':
                    continue
                else:
                    lines_without_comments.append(l)

            self.path_state = self.folder + lines_without_comments[0]
            self.path_action = self.folder + lines_without_comments[1]
            self.path_step_result = self.folder + lines_without_comments[2]
            self.continuous = bool(lines_without_comments[3])
            self.state_space = int(lines_without_comments[4])
            self.action_space = int(lines_without_comments[5])
            self.start_env_process = lines_without_comments[6]

        if self.continuous:
            self.action_space = spaces.Box(-1, +1, (self.action_space,), dtype=np.float32)
        else:
            self.action_space = spaces.Discrete(3)

        # cur_temp, target_temp, cur_set_point, cur_flow_rate
        self.observation_space = spaces.Box(-1, 1, shape=(self.state_space,), dtype=np.float32)

        self.steps = 0
        self.angle = 0


        self.np_random = None
        self.seed()
        self.viewer = None
        self.state = None
        self.all_states = list()

        self.screen_w = 500
        self.screen_h = 250
        self.x_gap = 5
        self.y_gap = 5
        self.scale = self.screen_w / self.screen_h

        self.steps_until_done = 560#80 160

        self.one_step_x_scale = (self.screen_w - 2 * self.x_gap) / self.steps_until_done
        self.one_step_temp_scale = (self.screen_h - 2 * self.y_gap) / 7
        self.one_step_set_point_scale = (self.screen_h - 2 * self.y_gap) / 7

        self.axes = list()
        # lower horiz
        #self.axes.append({'line': [(self.x_gap, self.y_gap), (self.screen_w - self.x_gap, self.y_gap)],
        #                  'color': (0, 0, 0)})
        # vert
        self.axes.append({'line': [(self.x_gap, self.y_gap), (self.x_gap, self.screen_h - self.y_gap)],
                          'color': (0, 0, 0)})
        # mid horiz
        self.axes.append({'line': [(self.x_gap, self.y_gap + self.screen_h/2), (self.screen_w - self.x_gap, self.y_gap + self.screen_h/2)],
                          'color': (0, 0, 0)})

        self.min_allow_temp = -3.5
        self.min_set_point = -3.5
        self.wait_sleep = 0.5

        self.bFirstRun = True

        # matplotlib plotting
        pyplot.ion()
        pyplot.show()

        self.figure = pyplot.figure(num='Worker'+str(self.rank))
        #self.figure.suptitle()
        self.time_data = []
        self.y = []
        self.Cd = []
        self.Cl = []

        self.statesPlot = [pyplot.plot(self.y, self.time_data, label='state' + str(i)) for i in range(self.state_space)]

        self.CdPlot, = pyplot.plot(self.Cd, self.time_data, 'b', label='<Fx>')
        self.ClPlot, = pyplot.plot(self.Cl, self.time_data, 'c', label='<Fy>')

        self.actionsPlot, = pyplot.plot(self.y, self.time_data, 'r', label='action')
        self.rewardsPlot, = pyplot.plot(self.y, self.time_data, 'g', label='reward')

        pyplot.legend(loc="upper left")
        #pyplot.ylim(2.4, 2.6)
        pyplot.draw()
        pyplot.pause(0.001)
        #self.animation = FuncAnimation(self.figure, self.myrender, interval=1000)

        # Fail test functioonlity
        self._step_started_at = None
        self._max_episode_seconds = 60#!!!!!!!!!!!!!!!!!!
        self.resetFailTestFlag()

        try:
            os.remove(self.path_step_result)
        except FileNotFoundError:
            pass
        try:
            os.remove(self.path_state)
        except FileNotFoundError:
            pass
        try:
            os.remove(self.path_action)
        except FileNotFoundError:
            pass

    def resetFailTestFlag(self):
        with open(self.folder + 'failTest.txt', "w") as f:
            f.write('0')

    def seed(self, seed=None):
        self.np_random, seed = seeding.np_random(seed)
        return [seed]

    # def step(self, action: float_or_int):
    def step(self, action):

        self.steps += 1

        failTest = 0
        with open(self.folder + 'failTest.txt', "r") as f:
            failTest = int(f.readline())

        if ( failTest == 0 ):
            with open(self.path_action, "w") as f:
                #random.gauss(0, 0.5) +
                #action[0] = 0#2 * math.sin( 2 * math.pi * 0.143 * 3 * 0.01 * ( self.startCFDstep + 6 + 30 * (self.steps - 3) ) )#random.random() * 4 - 2
                ctrl_value = action[0]
                ctrl_value2 = 3
                f.write('{0} {1}\n'.format(ctrl_value, ctrl_value2))
                print('worker={0} step={1} control values {2} and {3} were written\n'.format(self.rank, self.steps, ctrl_value, ctrl_value2))

        # HERE IS A LONG CFD CRUNCHING.............

        self._step_started_at = time.time()
        while(os.path.isfile(self.path_step_result)!=True or os.path.isfile(self.path_state)!=True):

            if ( self._elapsed_seconds > self._max_episode_seconds ):
                print('worker={0} step={1} failure detected...\n'.format(self.rank, self.steps))
                self.failProcessRecover()
                with open(self.path_action, "w") as f:
                    ctrl_value = action[0]
                    f.write('{} 0.6\n'.format(ctrl_value))
                    print('worker={0} step={1} control value {2} was written\n'.format(self.rank, self.steps, ctrl_value))
                print('worker={0} step={1} failure recovered...\n'.format(self.rank, self.steps))

            time.sleep(self.wait_sleep)
            print('step={0} waiting for the {1} and {2} for {3} of {4} seconds\n'.format(self.steps, self.path_step_result, self.path_state, int(self._elapsed_seconds ), self._max_episode_seconds ))

        self.angle += ctrl_value

        # file exists
        with open(self.path_step_result) as f, open(self.path_state) as f2:

            all_lines = f.readline()
            vals = list(all_lines.split())
            print('worker={0} step={1} obtained reward data <Fx>={2}, <Fy>={3}, steps={4}'.format(self.rank, self.steps, vals[0], vals[1], vals[2]))
            print('worker={0} step={1} calculated reward data ANGLE={2}'.format(self.rank, self.steps, self.angle))
            self.Cd.append(float(vals[0]))
            self.Cl.append(float(vals[1]))
            #reward = -(float(vals[0]))# + abs(float(vals[1])))
            #reward = 1.0/(float(vals[0]))

            #reward = 3 - (float(vals[0]) + 0.1 * abs(float(vals[1]))) #1
            reward = 3 - (float(vals[0]) + 0.2 * abs( float(vals[1]))) #2
            #reward = 3 - (float(vals[0]) + 0.01 * abs(self.angle))  #3

            all_lines = f2.read()
            lines = all_lines.split('\n')

            self.state = np.empty((self.state_space,), dtype=np.float32)
            for i in range(0, self.state_space):
                self.state[i] = float(lines[i])

            print('worker={0} step={1} obtained state={2}'.format(self.rank, self.steps, self.state))

        os.remove(self.path_step_result)
        os.remove(self.path_state)

        #raise Exception('{}'.format(done))  # debug
        self.all_states.append([self.state, (action, reward)])

        if self.steps == self.steps_until_done:
            done = True
        else:
            done = False
        print('worker={0} step={1} done={2}'.format(self.rank, self.steps, done))

        info = dict()

        return np.array(self.state), reward, done, info

    @property
    def _elapsed_seconds(self):
        return time.time() - self._step_started_at

    def startNewProcess(self):
        # Remember current directory
        cur_dir = os.path.abspath(".")
        os.chdir(self.folder)
        cur_dir2 = os.path.abspath(".")

        # Starting new CFD and remember its pid for waiting before exit next time...
        #res = os.system(self.start_env_process + ' & echo $! > pid.txt')
        #print('os.system returned: {}'.format(res))

        env = os.environ
        new_env = {k: v for k, v in iter(env.items()) if "MPI" not in k}
        p = subprocess.Popen(self.start_env_process + ' & echo $! > pid.txt', env=new_env, shell=True, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        p.wait()

        with open('pid.txt', 'r') as f:
            pid = int(f.readline())
            print('CFD was run with pid={}...'.format(pid))
            #time.sleep(30)

        os.chdir(cur_dir)

    def failProcessRecover(self):
        # kill failed Process
        with open(self.commonfolder + 'failLog.txt', 'a') as f:
            f.write(time.asctime() + ' failure detected for worker={0} at step={1}\n'.format(self.rank, self.steps))

        pid = 0
        with open(self.folder + 'pid.txt', 'r') as f:
            pid = int(f.readline())

        try:
            os.kill(pid, signal.SIGTERM)  # or signal.SIGKILL
        except ProcessLookupError:
            print('unable to terminate process pid={}...\n'.format(pid))
            with open(self.commonfolder + 'failLog.txt', 'a') as f:
                f.write(time.asctime() + ' unable to terminate process pid={0} for worker={1} at step={2}\n'.format(pid,
                self.rank, self.steps))

        self.startNewProcess()
        self.resetFailTestFlag()

        ################################
        # run the new process to the self.steps step

        for idx, [st, (a, r)] in enumerate(self.all_states):

            with open(self.path_action, "w") as f:
                # here just set 0 action, however further this means we need somehow to reset the environment
                ctrl_value = a[0]
                f.write('{} 0\n'.format(ctrl_value))
                print('worker={0} step={1} control value {2} was written during the failure recover\n'.format(self.rank, idx, ctrl_value))

            while (os.path.isfile(self.path_step_result) != True or os.path.isfile(self.path_state) != True):
                time.sleep(self.wait_sleep)
                print('worker={0} step={1} waiting for the {2} and {3} during the failure recover\n'.format(self.rank, idx,
                                                                                 self.path_step_result,
                                                                                 self.path_state))

            # file exists
            with open(self.path_step_result) as f, open(self.path_state) as f2:

                all_lines = f.readline()
                vals = list(all_lines.split())
                print('worker={0} step={1} obtained reward data <Fx>={2}, <Fy>={3}, steps={4}  during the failure recover\n'.format(self.rank,
                                                                                                      idx,
                                                                                                      vals[0],
                                                                                                      vals[1],
                                                                                                      vals[2]))

                all_lines = f2.read()
                lines = all_lines.split('\n')

                state = np.empty((self.state_space,), dtype=np.float32)
                for i in range(0, self.state_space):
                    state[i] = float(lines[i])
                print('worker={0} step={1} obtained state={2}  during the failure recover\n'.format(self.rank, idx, state))

            os.remove(self.path_step_result)
            os.remove(self.path_state)

        ###############################

        with open(self.commonfolder + 'failLog.txt', 'a') as f:
            f.write(time.asctime() + ' failure recovered for worker={0} at step={1}\n\n'.format(self.rank, self.steps))

        self._step_started_at = time.time()

    def reset(self):

        self.steps = 0
        self.angle = 0
        self.all_states = list()

        # Terminating previouse CFD
        if self.bFirstRun == False:
            # stop CFD
            with open(self.folder + 'exit_now', 'w') as f:
                pass

            # LET CFD GET EXIT_NOW...
            with open(self.path_action, "w") as f:
                # here just set 0 action, however further this means we need somehow to reset the environment
                f.write('0 0\n')

            pid = 0
            with open(self.folder + 'pid.txt', 'r') as f:
                pid = int(f.readline())

            print('Waiting for exiting CFD...')
            os.system('tail --pid={} -f /dev/null'.format(pid))
            #time.sleep(5)

        self.startNewProcess()

        # READY!!!

        # with open(self.path_action, "w") as f:
        #     # here just set 0 action, however further this means we need somehow to reset the environment
        #     f.write('0 0\n')

        self.bFirstRun = False

        while(os.path.isfile(self.path_step_result)!=True or os.path.isfile(self.path_state)!=True):
            time.sleep(self.wait_sleep)
            print('worker={0} step={1} waiting for the {2} and {3}\n'.format(self.rank, self.steps, self.path_step_result, self.path_state))

        # file exists
        with open(self.path_step_result) as f, open(self.path_state) as f2:

            all_lines = f.readline()
            vals = list(all_lines.split())
            print('worker={0} step obtained reward data <Fx>={1}, <Fy>={2}, steps={3}'.format(self.rank, vals[0], vals[1], vals[2]))
            reward = -float(vals[0])
            self.startCFDstep = float(vals[2])

            all_lines = f2.read()
            lines = all_lines.split('\n')

            self.state = np.empty((self.state_space,), dtype=np.float32)
            for i in range(0, self.state_space):
                self.state[i] = float(lines[i])

            print('worker={0} step obtained state={1}'.format(self.rank, self.state))

        os.remove(self.path_step_result)
        os.remove(self.path_state)

        # while(os.path.isfile(self.path_state)!=True):
        #     time.sleep(self.wait_sleep)
        #     print('reset waiting for the {}\n'.format(self.path_step_result))
        #
        # # wait until the file is full
        # time.sleep(self.wait_sleep)

        # # file exists
        # with open(self.path_state) as f:
        #
        #     all_lines = f.read()
        #     lines = all_lines.split('\n')
        #
        #     self.state = np.empty((self.state_space,), dtype=np.float32)
        #     for i in range(0, self.state_space):
        #         self.state[i] = float(lines[i])
        #
        #     print('reset obtained state={0}'.format(self.state))
        #
        # os.remove(self.path_state)

        self.Cd = []
        self.Cl = []

        self.all_states.append([self.state, (np.zeros((1,)), np.zeros((1,)))])
        return np.array(self.state)

    # def render(self, mode='human'):
    #     if self.viewer is None:
    #         from gym.envs.classic_control import rendering
    #         self.viewer = rendering.Viewer(self.screen_w, self.screen_h)
    #
    #     all_ct, all_tt, all_sp, all_fr, all_ia, all_a = list(), list(), list(), list(), list(), list()
    #     for idx, [st, (a, r)] in enumerate(self.all_states):
    #         (ct, tt, ss) = (st[0], -r, st[0])
    #         all_ct.append([self.x_gap + int(idx * self.one_step_x_scale),
    #                        int(self.y_gap + (ct - self.min_allow_temp) * self.one_step_temp_scale)])
    #         all_tt.append([self.x_gap + int(idx * self.one_step_x_scale),
    #                        int(self.y_gap + (tt - self.min_allow_temp) * self.one_step_temp_scale)])
    #         # all_ia.append(ia)
    #         all_a.append([self.x_gap + int(idx * self.one_step_x_scale),
    #                        int(self.y_gap + (a[0] - self.min_set_point) * self.one_step_set_point_scale)])
    #
    #     if len(self.all_states) == self.steps_until_done:
    #         breakpoint = True
    #         # print(all_ia)
    #
    #     all_ct = ({'line': all_ct, 'color': (0, 1, 0)})
    #     all_tt = ({'line': all_tt, 'color': (1, 0, 0)})
    #     all_a = ({'line': all_a, 'color': (0.75, 0.75, 1)})
    #     # all_sp = ({'line': all_sp, 'color': (0.8, 0.8, 0.8)})
    #     # all_fr = ({'line': all_fr, 'color': (0, 0, 1)})
    #     # lines = [all_sp, all_ct, all_tt]
    #     lines = [all_a, all_ct, all_tt]
    #
    #     objs = list()
    #     objs.extend(self.axes)
    #     objs.extend(lines)
    #     for obj in objs:
    #         self.viewer.draw_polyline(obj['line'], color=obj['color'], linewidth=2)
    #
    #     if self.state is None:
    #         return None
    #

    def render(self, mode='human'):

        times = list()
        actions = list()
        rewards = list()
        slatesList = list()

        for idx, [st, (a, r)] in enumerate(self.all_states):
            # 0 data corresponds to reset so skip it
            if idx == 0:
                continue
            times.append(idx)
            actions.append(a[0])
            rewards.append(r)

        for i in range(self.state_space):
            states = list()
            for idx, [st, (a, r)] in enumerate(self.all_states):
                # 0 data corresponds to reset so skip it
                if idx == 0:
                    continue
                states.append(st[i])

            slatesList.append(states)

        for i in range(self.state_space):
            self.statesPlot[i][0].set_data(times, slatesList[i])

        self.CdPlot.set_data(times, self.Cd)
        self.ClPlot.set_data(times, self.Cl)

        self.actionsPlot.set_data(times, actions)
        self.rewardsPlot.set_data(times, rewards)

        self.figure.gca().relim()
        self.figure.gca().autoscale_view()

        print('worker={0} render plots...'.format(self.rank))
        pyplot.draw()
        pyplot.pause(0.001)

        # if len(self.all_states) == self.steps_until_done:
        #     breakpoint = True

    def close(self):

        print('close(self) called')

        # Terminating previouse CFD
        #if self.bFirstRun == False:
        # stop CFD
        with open(self.folder + 'exit_now', 'w') as f:
            pass

        # LET CFD GET EXIT_NOW...
        with open(self.path_action, "w") as f:
            # here just set 0 action, however further this means we need somehow to reset the environment
            f.write('0 0\n')

        pid = 0
        with open(self.folder + 'pid.txt', 'r') as f:
            pid = int(f.readline())

        print('Waiting for exiting CFD...{}'.format(pid))
        os.system('tail --pid={} -f /dev/null'.format(pid))

        #with open(self.path_action,"w+") as f:
        #   f.write('3 0\n')

        if self.viewer:
            self.viewer.close()
            self.viewer = None
