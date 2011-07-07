#!/usr/bin/perl
#
#   Copyright (c) 2011, Nokia Corporation
#   All Rights Reserved.
#
#    Redistribution and use in source and binary forms, with or without
#    modification, are permitted provided that the following conditions
#    are met:
# 
#     * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
#     * Neither the name of Nokia nor the names of its contributors
#     may be used to endorse or promote products derived from this
#     software without specific prior written permission.
# 
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
#   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
#   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
#   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
#   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
#   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
#   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
#
# @author Dmitry Kolesnikov <dmitry.kolesnikov@nokia.com>
#
use Getopt::Std;

sub usage
{
   print STDERR "\n$0 [OPTIONS] OUTPUT\n";
   print STDERR "\t-s sec  \tsample interval (default 60)\n";
   print STDERR "\t-c class\tdata stream class (default GAUGE)\n";
}
my %opts=();
getopts("s:c:",\%opts);
if (!defined $ARGV[0]) 
{
   usage();
   exit(-1);
}

my $db    = $ARGV[0];
my $hbeat = $opts{s} if (defined $opts{s});
my $hbeat = 60       if (!defined $opts{s});
my $class = $opts{c} if (defined $opts{c});
my $class = "GAUGE"  if (!defined $opts{c});
my $tout  = $hbeat * 3;  


$flags = " -s $hbeat ";                           
$def   = " DS:value:GAUGE:$tout:U:U ";
#
# archive, default schema
# 1      to 1 during week
# 15 min to 1 during month
# 30 min to 1 during year
#
$spw  = int(7  * 24 * 3600 / $hbeat); #steps per week
$spq  = int(15 * 60 / $hbeat);        #steps in quater
$qm   = int(31 * 24 * 60 / 15);       #quaters in month
$sph  = int(30 * 60 / $hbeat);        #steps in half
$hy   = int(365 * 24 * 60 / 30);      #halves in year
$rra  = " RRA:AVERAGE:0.9999:1:$spw ";
$rra .= " RRA:AVERAGE:0.9999:$spq:$qm ";  #15min to 1 
$rra .= " RRA:AVERAGE:0.9999:$spq:$hy ";

system("rrdtool create $db $flags $def $rra");
print "Error: $!\n" if ( $? == -1 );

