#
# Copyright 2020 Paul Onions
# Licence: MIT, see LICENCE file for details.
#
namespace eval ::sb-tcl:: {

    namespace export COMPLEX STRING LIST VECTOR

    ##
    ## Utility procedures for creating READable Lisp strings
    ##
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
}
