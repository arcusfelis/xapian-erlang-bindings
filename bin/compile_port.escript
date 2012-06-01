#!/usr/bin/env escript


main([]) ->
    %% TODO: escape
    ScriptDir = filename:dirname(escript:script_name()),
    CDir        = ScriptDir ++ "/../c_src/",
    Out         = ScriptDir ++ "/../priv/xapian_port",
    AllObjFiles = filelib:wildcard(CDir ++ "*.o"), 
    DrvFiles    = filelib:wildcard(CDir ++ "*drv*.o"), 
    ObjFiles    = AllObjFiles -- DrvFiles,

    XapianConfig = os:cmd("xapian-config --libs"),
    CompileCommand = "gcc -o " 
                ++ Out 
                ++ string:join(["" | ObjFiles], " ") 
                ++ " -lstdc++ "
                ++ XapianConfig,
    io:format("Compile command: ~s~n", [CompileCommand]),
    Result = os:cmd(CompileCommand),
    io:format("Compile result: ~s~n", [Result]).


