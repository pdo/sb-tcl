##
## Copyright (c) 2016 - 2018 Paul Onions
## Licence: MIT, see LICENCE file for details
##
## A Tcl REPL for testing purposes.
##
package require Tk

set cmdString ""

proc openReplWindow { container } {
    frame $container.cmdFrame
    label $container.cmdFrame.cmdLabel -text "Command:"
    entry $container.cmdFrame.cmd -textvariable cmdString
    button $container.cmdFrame.doCmd -text "Eval" -command [list doReplCmd $container]
    bind $container.cmdFrame.cmd "<Return>" "$container.cmdFrame.doCmd invoke"
    pack $container.cmdFrame.cmdLabel -side left
    pack $container.cmdFrame.cmd      -side left -fill x -expand yes
    pack $container.cmdFrame.doCmd    -side right

    frame $container.outputFrame
    text $container.outputFrame.output -height 10 -width 80 -relief raised \
        -xscrollcommand "$container.outputFrame.xscroll set" \
        -yscrollcommand "$container.outputFrame.yscroll set"
    scrollbar $container.outputFrame.xscroll -orient horizontal \
        -command "$container.outputFrame.output xview"
    scrollbar $container.outputFrame.yscroll -orient vertical \
        -command "$container.outputFrame.output yview"
    pack $container.outputFrame.yscroll -side right  -fill y
    pack $container.outputFrame.xscroll -side bottom -fill x
    pack $container.outputFrame.output  -expand yes  -fill both

    pack $container.cmdFrame    -fill x
    pack $container.outputFrame -fill both -expand yes
}

proc doReplCmd { container } {
    global cmdString
    set result [uplevel #0 [list eval $cmdString]]
    $container.outputFrame.output insert end "\n"
    $container.outputFrame.output insert end $result
    $container.outputFrame.output insert end "\n"
    $container.outputFrame.output see end
}

# Example invocation:-
#
# openReplWindow [toplevel .repl]
