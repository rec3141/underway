#!/usr/bin/perl
use warnings;
use strict;
use lib '../blib/lib','../blib/arch';
use IO::Socket::Multicast;
use Data::Dumper;

my $sock = IO::Socket::Multicast->new(LocalPort=>'40119');
#print Dumper($sock);
$sock->mcast_add('232.255.255.255') || die "Couldn't set group: $!\n";

my $data;
while(1) {
	$sock->recv($data,3096);
	print $data;
}
