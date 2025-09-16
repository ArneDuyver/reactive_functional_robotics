# Functional Reactive Robotics
_**A Haskell/Yampa based robotics framework based around Finite State Machines and MQTT**_

## TODO:
- [ ] Create input and output data type builders
  - [ ] Also create default values that are used standard by the program if the user does not specify one in the configuration file
  - [ ] Make the decoding not throw an error if non critical in builder
- [ ] Split up functions into libraries/library project
  - [ ] State template function with dict and make non critical controllers non critical otherwise throw an error -> Monads?
- [ ] Test time sensitivity and resource hogging against equivalent in **ROS2**
- [ ] Test time sensitivity and resource hogging against equivalent in **Rust**
- [ ] Make builder not overwrite existing files? or give it as an option (checkbox in frontend)
- [ ] Frontend upload button voor fsm.xml file

### MOL:
- [ ] Script to install cabal and haskell
- [ ] Script to start and open frontend

### Other questions/work:
- [ ] Is a default value for an input always required?
- [ ] Do output values need a default value?
- [ ] Can outputs also be non-critical?
- [ ] Other things to put in configuration.yml?
  - [ ] Time-interval between mqtt messages?

### Extra library functionalities:
- [ ] SLAM
- [ ] 

## Explanation of _Functional Reactive Robotics_:
**MessageCollector**: collects all different MQTT input topics. Saves last entry for each one and combines them into 1 message to send to the FSM environment.

**FsmBuilder**: uses the FSM schema (TODO: and a components.yml file for all inputs and outputs) to build the relevant FSM files into the FsmRunner.

**FsmFrontend**: 
- _build_: run the FsmBuilder cabal files
- _run_: run the FsmRunner cabal files (TODO: also run the MessageCollector via the FsmRunner)
- _quit_: (TODO: quit the FsmRunner????)
- _clear_: clear output in the div

**config/.env**: contains all environment variables for the different MQTT connections

**config/fsm.xml**: contains the XML export of the Draw.io schema of the FSM

**config/configuration.yml**: contains all controller inputs and outputs settings