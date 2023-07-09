#lang scribble/manual

@title{2048}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]

@defmodule[2048/main/gui/2048]

This package provides a Racket version of the game 2048.
It currently works as a GUI application,
with a launcher installed by @exec{raco setup}.
Support for a text-based interface is planned in the future.

The game is inspired by Gabriele Cirulli's
@hyperlink["https://github.com/gabrielecirulli/2048"]{JavaScript version}:
the implementations don't share any code, but, for example,
the values of certain constants are the same as in Cirulli's version,
so the difficulty level should be comparable.

@section{License}

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but @bold{without any waranty;} without even the implied warranty of
@bold{merchantability} or @bold{fitness for a particular purpose.}
See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see @url["https://www.gnu.org/licenses/"].
