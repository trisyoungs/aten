#!/usr/bin/perl -w
$/="---\n";
while ($r=<>) {
  $r=~s/^\-*$//gms;
  ($rev, $comment) = ($r =~ m/r([0-9]*).*?\n\n(.*)\n-*/s);
  if (!$rev) {next;}
  $comment =~ s/^\s*$//msg;
  $comment =~ s/(?<!etc)\. {1,3}/.\n\t\t<listitem>\n\t\t\t<para>/g;
  $comment =~ s/\.$/.<\/para>\n\t\t<\/listitem>/msg;
  $commenthash{$rev} = $comment;

}

# Print out shite
foreach $rev (reverse(sort(keys(%commenthash)))) {
  print "\t<para><emphasis>Revision $rev</emphasis></para>\n\t<itemizedlist mark='opencircle'>\n\t\t<listitem>\n\t\t\t<para>$commenthash{$rev}\t</itemizedlist>\n\n";
}
