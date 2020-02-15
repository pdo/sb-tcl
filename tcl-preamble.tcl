#
# Copyright 2020 Paul Onions
# Licence: MIT, see LICENCE file for details.
#
namespace eval ::sb-tcl:: {

    #
    # Utility procedures for creating READable Lisp strings
    #
    namespace export COMPLEX STRING LIST VECTOR

    proc COMPLEX {x y} {
        concat "#C(" $x $y ")"
    }

    proc STRING {str} {
        string cat \" $str \"
    }

    proc LIST {args} {
        concat "(" {*}$args ")"
    }

    proc VECTOR {args} {
        concat "#(" {*}$args ")"
    }

    #
    # Procedures to setup remotely accessible Tcl REPLs
    #
    namespace export start_repl_server stop_repl_server

    variable repl_server
    variable repl_number 0

    proc repl_prompt {chan prompt} {
        puts -nonewline $chan $prompt
        flush $chan
    }

    proc repl_handler {chan cmdvar donevar} {
        variable $cmdvar
        variable $donevar
        if {[gets $chan line] < 0} {
            chan event $chan readable {}
            set [namespace current]::$donevar 1
            close $chan
            return
        }
        append $cmdvar $line
        if {[info complete [set $cmdvar]]} {
            chan event $chan readable {}
            set status [catch {uplevel #0 [set $cmdvar]} result]
            chan event $chan readable [list [namespace current]::repl_handler $chan $cmdvar $donevar]
            if {$result ne ""} {
                puts $chan $result
            }
            set $cmdvar ""
            repl_prompt $chan "% "
        } else {
            append $cmdvar \n
            repl_prompt $chan "> "
        }
    }

    proc start_repl {sock addr port} {
        variable repl_number
        set n [expr $repl_number + 1]
        set repl_number $n
        variable repl${n}_cmd  ""
        variable repl${n}_done 0
        chan configure $sock -buffering line -encoding utf-8 \
            -blocking 0 -translation crlf
        repl_prompt $sock "% "
        chan event $sock readable [list [namespace current]::repl_handler $sock repl${n}_cmd repl${n}_done]
    }

    proc start_repl_server {{port 0}} {
        variable repl_server [socket -server ::sb-tcl::start_repl $port]
        set portnum [lindex [chan configure $repl_server -sockname] 2]
        return $portnum
    }

    proc stop_repl_server {} {
        variable repl_server
        close $repl_server
    }
}
