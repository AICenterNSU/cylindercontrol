import os.path as osp


def custom_envs_train_parameters(args, extra_args):
    load_model = None
    # ENV + PARAMETERS
    args.env = "VirtualEnv"
    args.env_steps = 512
    args.env_mode = None
    args.env_cont = None
    args.env_norm = None
    # -- OR --
    #args.env = "LunarLanderContinuous-v2"

    experiment_name = 'tests'

    args.num_env = 1;#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!5 3 1
    args.start_index = 0;#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 0 if num_env == 1
    args.alg = "ppo2"
    args.num_timesteps = 1e8

    # TRAIN ALGORITHM + PARAMETERS
    args.network = "mlp"
    extra_args['num_hidden'] = 64
    extra_args['num_layers'] = 2
    # -- OR --
    # args.network = "lstm"
    # extra_args["nlstm"] = 1024

    extra_args['ent_coef'] = 0.01
    extra_args['value_network'] = "copy"
    extra_args['nsteps'] = 560#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!320(80) 560(80) 1600(80) 480(160)
    # extra_args['cliprange'] = 0.2

    extra_args['log_interval'] = 1
    extra_args['save_interval'] = 1

    if not args.play:
        load_model = False
    # to load model
    #load_model = True#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (not args.play and load_model) or (args.play and 'load_path' not in extra_args):
        # args.env_cont = True
        # args.env_norm = True
        #timestamp_dir = '20200610-013137_dns_re100_entropy0.01_3-cd-0.1cl_long'
        timestamp_dir = '20200622-190846_dns_re100_entropy0.01_3-cd-0.2cl_50long'
        #timestamp_dir = '20200625-163407_dns_re100_entropy0.01_3-cd-0.2cl_50-71long'
        #timestamp_dir = '20200629-224550_dns_re100_entropy0.01_3-cd-0.01angle_129long'
        checkpoint_name = '00117'

        extra_args['load_path'] = osp.join('train_data', args.env, experiment_name, timestamp_dir, 'checkpoints',
                                           checkpoint_name)

    return args, extra_args, experiment_name
