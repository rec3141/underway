#!/usr/bin/perl

### REALTIME-UDP-RCV.SH
### by Eric Collins (recollins@alaska.edu), University of Alaska Fairbanks
### last updated 8 August 2016

### This program acquires realtime data from the onboard UDP multicast
### There is a separate program, realtime-runner.r, that calls this program and reads its output

use warnings;
use strict;
use lib '../blib/lib','../blib/arch';
use IO::Socket::Multicast;
#use Data::Dumper;

# this port is set in the MetACQ settings
my $sock = IO::Socket::Multicast->new(LocalPort=>'40119');
#print Dumper($sock);
$sock->mcast_add('232.255.255.255') || die "Couldn't set group: $!\n";

my $data;
while(1) {
	$sock->recv($data,1028);
	print $data;
}
