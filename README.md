# cylindercontrol

This repository includes codes for numerical cylinder flow control cases.

## Low Reynolds (Re=100) cylinder flow control with rotary oscillations by DRL optimization algorithm

The folder rotaryRe100DRL contains a DRL agent based on the original [OpenAI baselines](https://github.com/openai/baselines) implementation of the PPO algorithm.

The graphical abstract is available at https://youtu.be/9X8XtHk0R84

### Building CFD environment and the control coupling codes

- In order to run training before you have to download and build TFlows CFD solver [here](https://github.com/DelNov/T-Flows) from the development_branch.

- The solver Processor can be built with the follolwing command:
```bash
make MPI=yes FCOMP=mpif90.openmpi CGNS=no CGNS_MPI=openmpi DIR_CASE=<folder_with_the_agent>
```
- Change the text inside the brackets with the folder name containing User_Mod folder coupling the Fortran code with the DLR agent.
After successfull build create a symbolic link to the Processor binaries inside the DIR_CASE.

### Running training

- Run python module to start the agent

```bash
cd OpenAI-baselines-CustomEnvs
python -m baselines.run
```

- Training log with be written to a train_data subfolder with correspondong current models of the controllers 

- Use tensorboard for monitoring the training progress

### Testing trained model

- Use the follwing code to test a trained model

```bash
python -m baselines.run --load_path=<absolute_path_to_the_trained_model>
```

- Use pretrained model to see the control in action

```bash
python -m baselines.run --load_path=<absolute_path>/cylindercontrol/rotaryRe100DRL/OpenAI-baselines-CustomEnvs/trained_models/VirtualEnv/20200622-190846_dns_re100_entropy0.01_3-cd-0.2cl_50long/checkpoints/00050
```

### Other issues

- Original python conda environment used for the project can be restored using environment.yml file, see directions [here](https://conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#create-env-from-file)

- Most of the training parametres can be modified in /CustomEnvs/CustomEnvs.py and /CustomEnvs/VirtualEnv/VirtualEnv.py files.
