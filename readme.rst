GMP, MPFR and MPC interface library for gcc-Ada (GNAT)
======================================================

What's this?
------------

Ada binding to the multiple precision arithmetic libraries GMP, MPFR and MPC.

Prerequisites
-------------

GCC >= 4.7
 https://gcc.gnu.org/
GMP >= 4.3
 http://gmplib.org/
MPFR >= 3.0
 http://www.mpfr.org/
MPC >= 0.9
 http://www.multiprecision.org/
headmaster
 http://github.com/ytomino/headmaster

Usage
-----

1. Translate C headers with headmaster. ::
   
    $ headmaster --to ada -p -D import-dir gmp-ada/source/import.h

2. Add source directories of gmp-ada and translated headers
   to search path for gnatmake. ::
   
    $ gnatmake -Igmp-ada/source -Iimport-dir your_main.adb
   
   Or please write .gpr file for your environment.

License
-------

It is dual-licensed under the New BSD License and LGPLv3, see below.
Please apply LGPLv3 when static linking libgmp.a, libmpfr.a or libmpc.a.

**license of gmp-ada (1)** ::

 Copyright 2010-2022 YT. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

**license of gmp-ada (2), GMP, MPFR and MPC** ::

 This file is part of gmp-ada.
 
 gmp-ada is free software: you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.
 
 gmp-ada is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public License
 along with gmp-ada.  If not, see <http://www.gnu.org/licenses/>.
