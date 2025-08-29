**MessageCollector**: collects all different MQTT input topics. Saves last entry for each one and combines them into 1 message to send to the FSM environment.

**FsmBuilder**: uses the FSM schema (TODO: and a components.yml file for all inputs and outputs) to build the relevant FSM files into the FsmRunner.

**FsmFrontend**: 
- _build_: run the FsmBuilder cabal files
- _run_: run the FsmRunner cabal files (TODO: also run the MessageCollector via the FsmRunner)
- _quit_: (TODO: quit the FsmRunner????)
- _clear_: clear output in the div

**config/.env**: contains all environment variables for the different MQTT connections

**config/fsm.xml**: contains the XML export of the Draw.io schema of the FSM