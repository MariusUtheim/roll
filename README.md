# roll

Usage:

    roll <cmd>
    
    roll -c
    
    roll -v cmd

Command specification

    <cmd1>+<cmd2> - sum the results of cmd1 and cmd2
    
    (n)d(s) - roll n s-sided dice
    
    (k) - return the constant number k, equivalent to kd1
    
    (n)d(s)d(d) - roll n s-sided dice, then drop the d smallest ones
    
    (m)x<cmd> - execute cmd m times and sum the results
    
    !<cmd> - execute the command verbosely, showing intermediate results
    
    -c, --character - roll a character. Equivalent to 6x4d6d1
    
    -v, --verbose <cmd> - execute the command verbosely. Equivalent to !cmd
    
