##============================================================================##
##                                                                            ##
##   AZ::Splitter Makefile maker                                              ##
##                                                                            ##
##============================================================================##

use 5.005;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME           => "AZ::Splitter",
    VERSION_FROM   => "lib/AZ/Splitter.pm",
    ABSTRACT_FROM  => "lib/AZ/Splitter.pm",
    AUTHOR         => "Andrian Zubko aka Ondr, <ondr\@mail.ru>"
);
