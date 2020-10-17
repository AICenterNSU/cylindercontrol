from .VirtualEnv import VirtualEnv
from gym.wrappers.time_limit import TimeLimit


def make_virtual_env(steps_until_done=400, rank=0):
    env = VirtualEnv(rank)
    # env = TimeLimit(env,
    #                 max_episode_steps=steps_until_done,
    #                 max_episode_seconds=steps_until_done)

    return env
