function _run_compleat
    set -x COMP_LINE (commandline)
    set -x COMP_POINT (commandline -C)
    set -x COMPLEAT_IS_FISH ""
    compleat $argv
end

[ -n "$COMPLEAT_SYSTEM_DIR" ]; or set COMPLEAT_SYSTEM_DIR /etc/compleat.d
[ -n "$COMPLEAT_USER_DIR"   ]; or set COMPLEAT_USER_DIR $HOME/.compleat

for DIR in $COMPLEAT_SYSTEM_DIR $COMPLEAT_USER_DIR
    if [ -d $DIR -a -r $DIR -a -x $DIR ]
        for FILE in $DIR/*.usage
            for COMMAND in (compleat $FILE)
                complete -c $COMMAND -a "(_run_compleat $FILE $COMMAND)"
            end
        end
    end
end
