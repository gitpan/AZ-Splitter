package AZ;

use 5.005;
use strict;
use Exporter;

##
##   Define system flags
##

our $_s_append = 0b00000001;
our $_s_wcase  = 0b00000010;
our $_s_wend   = 0b00000100;
our $_s_2buff  = 0b00001000;
our $_s_utfok  = 0b00010000;

##
##   Functions-flags in AZ:: namespace
##

sub s_APPEND { $_s_append }
sub s_WCASE  { $_s_wcase  }
sub s_WEND   { $_s_wend   }
sub s_2BUFF  { $_s_2buff  }
sub s_UTFOK  { $_s_utfok  }

##============================================================================##

package AZ::Splitter;

use autouse Carp => qw(croak);

our @ISA     = qw(Exporter);
our @EXPORT  = qw(s_APPEND s_WCASE s_WEND s_2BUFF s_UTFOK);

our $VERSION = 0.58;

##============================================================================##
##                                                                            ##
##   Public methods and functions                                             ##
##                                                                            ##
##============================================================================##

##
##   Functions-flags for export
##

sub s_APPEND { $_s_append }
sub s_WCASE  { $_s_wcase  }
sub s_WEND   { $_s_wend   }
sub s_2BUFF  { $_s_2buff  }
sub s_UTFOK  { $_s_utfok  }

##
##   Constructor
##

sub new
{
    croak("Usage: new AZ::Splitter(STREAM_IN, [SIZE_LIMIT, BUFF_SIZE, FLAGS])")
      unless(@_ >= 2 and @_ <= 5);

    my ($class, $inref, $inlimit, $buffsize, $flags) = @_;
    my ($s_2buff, $s_utfok, $self) = qw(0 0);

    if ($flags) {
        $s_2buff = $flags & $_s_2buff;
        $s_utfok = $flags & $_s_utfok;
    }
    $inlimit  = 1e10
      unless defined($inlimit)  and $inlimit  >= 0;
    $buffsize = 32*1024
      unless defined($buffsize) and $buffsize >= 0;

    $self = {
                inref    => $inref,
                inlimit  => $inlimit,
                inlpos   => 0,
                buffer   => "",
                buffer2  => "",
                buffsize => $buffsize,
                l_rsize  => 0,
                l_wsize  => 0,
                l_match  => "",
                s_2buff  => $s_2buff,
                s_utfok  => $s_utfok,
                status   => 1
            };

    return bless($self, $class);
}

##
##   Statistics
##

sub stat_rsize {
    croak("Usage: OBJECT->stat_rsize()") unless(@_ == 1);
    return $_[0]->{l_rsize};
}

sub stat_wsize {
    croak("Usage: OBJECT->stat_wsize()") unless(@_ == 1);
    return $_[0]->{l_wsize};
}

sub stat_match {
    croak("Usage: OBJECT->stat_match()") unless(@_ == 1);
    return $_[0]->{l_match};
}

sub stat_error {
    croak("Usage: OBJECT->stat_error()") unless(@_ == 1);
    return defined($_[0]->{status})? 0 : 1;
}

##
##   Method read_to()
##

sub read_to
{
    croak("Usage: OBJECT->read_to(STREAM_OUT, SEPARATORS, [MAX_SIZE, FLAGS])")
      unless(@_ >= 3 and @_ <= 5);

    my ($self, $outref, $seps, $max_size, $flags) = @_;
    my ($s_wcase, $s_wend, $s_append, @seps)      = qw(0 0 0);
    my ($no_errors, $max_bsize, $rsize)           = qw(1 0);

    if ($flags) {
        $s_wcase  = $flags & $_s_wcase;
        $s_append = $flags & $_s_append;
        $s_wend   = $flags & $_s_wend;
    }
    $max_size = 1e10
      unless defined($max_size) and $max_size >= 0;

    if (ref($seps) eq "ARRAY") {
    for (@$seps) {
        $max_bsize = length if length > $max_bsize;
        push(@seps, lc) if $s_wcase;
    }
    } else {
        $max_bsize = length($seps);
        @seps = lc($seps) if $s_wcase;
        $seps = [ $seps ];
    }

    $self->_init($outref, $s_append);
    return($self->{status}? 1 : 0)
      unless $self->{status} and $max_bsize;

    # Read data from stream through
    # buffer before first found separator
    for (;;)
    {
        # Search first separator in buffer
        if (length($self->{buffer}) >= $max_bsize)
        {
            my ($f_pos, $f_ind, $pos, $psize, $buffer) = 1e10;

            # Find first separator in buffer
            $buffer = \($s_wcase? ($self->{s_2buff}?
              $self->{buffer2} : lc($self->{buffer})) : $self->{buffer});

            for (0 .. @$seps - 1) {
                $pos = index($$buffer, $s_wcase? $seps[$_] : $seps->[$_]);
                ($f_ind, $f_pos) = ($_, $pos) if($pos != -1 and $pos < $f_pos);
            }
            # If found, then move part into result stream
            # and break with status code 0
            if ($f_pos < 1E10) {
                if ($no_errors and $self->{l_rsize} < $max_size) {
                    if (($rsize = $f_pos) > $max_size - $self->{l_rsize}) {
                        $rsize = $max_size - $self->{l_rsize};
                    }
                    $no_errors = _store($outref, \(substr($self->{buffer}, 0, $rsize)));
                    $self->{l_rsize} += $rsize if $no_errors;
                }
                $self->{l_wsize} += $f_pos;
                $self->{l_match}  = $seps->[$f_ind];
                $psize = $f_pos + length($seps->[$f_ind]);
                substr($self->{buffer},  0, $psize, "");
                substr($self->{buffer2}, 0, $psize, "") if $self->{s_2buff};
                return 1;
            }
            # Else move part of buffer to result stream without last
            # $max_bsize-1 characters at right side of buffer
            else {
                $psize = length($self->{buffer}) - $max_bsize + 1;
                if ($no_errors and $self->{l_rsize} < $max_size) {
                    if (($rsize = $psize) > $max_size - $self->{l_rsize}) {
                        $rsize = $max_size - $self->{l_rsize};
                    }
                    $no_errors = _store($outref, \(substr($self->{buffer}, 0, $rsize)));
                    $self->{l_rsize} += $rsize if $no_errors;
                }
                $self->{l_wsize} += $psize;
                substr($self->{buffer},  0, $psize, "");
                substr($self->{buffer2}, 0, $psize, "") if $self->{s_2buff};
            }
        }
        # If buffer empty and nothing more data in stream
        # then move last data to output stream and break cycle
        if (not $self->{inlimit}) {
            if (length($self->{buffer})) {
                if ($no_errors and $self->{l_rsize} < $max_size) {
                    if (($rsize = length($self->{buffer})) > $max_size - $self->{l_rsize}) {
                        $rsize = $max_size - $self->{l_rsize};
                    }
                    $no_errors = _store($outref, \(substr($self->{buffer}, 0, $rsize)));
                    $self->{l_rsize} += $rsize if $no_errors;
                }
                $self->{l_wsize} += length($self->{buffer});
                $self->{buffer}   = "";
                $self->{buffer2}  = "" if $self->{s_2buff};
            }
            $self->{status} = 0;
            return $s_wend? 0 : 1;
        }
        # Trying to fill buffer
        else {
            $self->_fill_buffer() or do { undef($self->{status}); return 0 };
            if ($self->{s_2buff}) {
                $self->{buffer2} = lc($self->{buffer});
            }
        }
    }
}

##
##   Method read_some()
##

sub read_some
{
    croak("Usage: OBJECT->read_some(STREAM_OUT, [MAX_SIZE, FLAGS])")
      unless(@_ >= 2 and @_ <= 4);

    my ($self, $outref, $max_size, $flags) = @_;
    my ($no_errors, $s_append, $rsize)     = qw(1 0);

    if ($flags) {
        $s_append = $flags & $_s_append;
    }
    $max_size = 1e10
      unless defined($max_size) and $max_size >= 0;

    $self->_init($outref, $s_append);
    return 0 unless $self->{status};

    # Read from stream through buffer
    while ($self->{l_wsize} < $max_size)
    {
        # Trying to fill buffer
        if (not length($self->{buffer}))
        {
            $self->_fill_buffer() or do { undef($self->{status}); return 0 };
            if ($self->{s_2buff}) {
                $self->{buffer2} = lc($self->{buffer});
            }
        }
        # Well, if buffer still empty then break cycle
        if (not length($self->{buffer}))
        {
            $self->{status} = 0;
            return $self->{l_wsize}? 1 : 0;
        }
        # If not enouth data in buffer, move all to output stream
        elsif (($rsize = length($self->{buffer})) <= $max_size - $self->{l_wsize})
        {
            if ($no_errors) {
                $no_errors = _store($outref, \$self->{buffer});
            }
            $self->{l_rsize} += $rsize if $no_errors;
            $self->{l_wsize} += $rsize;
            $self->{buffer}   = "";
            $self->{buffer2}  = "" if $self->{s_2buff};
        }
        # Move necessary quantity of characters from buffer to output
        # stream and break cycle
        else {
            $rsize = $max_size - $self->{l_wsize};
            if ($no_errors) {
                $no_errors = _store($outref, \(substr($self->{buffer}, 0, $rsize)));
            }
            $self->{l_rsize} += $rsize if $no_errors;
            $self->{l_wsize} += $rsize;
            substr($self->{buffer},  0, $rsize, "");
            substr($self->{buffer2}, 0, $rsize, "") if $self->{s_2buff};
            return 1;
        }
    }

    return 1;
}

##============================================================================##
##                                                                            ##
##   Private methods and functions                                            ##
##                                                                            ##
##============================================================================##

##
##   Trying to fill buffer. Returns zero on errors
##   Usage: BOOL = SELF->_fill_buffer()
##

sub _fill_buffer
{
    my ($self, $rsize, $buffsize) = shift;

    if ($self->{inlimit})
    {
        # Reading part of data from input stream, depending on type
        # of the stream reference
        $buffsize = $self->{inlimit}
          unless ($buffsize = $self->{buffsize}) <= $self->{inlimit};

        if (ref($self->{inref}) eq "SCALAR")
        {
            # Calculate length of characters
            $rsize = length(${$self->{inref}}) - $self->{inlpos};
            $rsize = $buffsize if $rsize > $buffsize;
            $rsize = 0         if $rsize < 0;

            # Get data from string by reference
            $self->{buffer} .= substr(${$self->{inref}}, $self->{inlpos}, $rsize);
            $self->{inlpos} += $rsize;
        }
        elsif (ref($self->{inref}) eq "GLOB")
        {
            # Get data from file
            $rsize = read($self->{inref},
              substr($self->{buffer}, length($self->{buffer})), $buffsize);
        }

        # Correct size of stream limit if were no errors.
        # Else erase buffer and break with zero status
        if (not defined($rsize)
        or
        $] > 5.007 and not $self->{s_utfok} and $rsize
        and utf8::is_utf8($self->{buffer}) and not utf8::valid($self->{buffer})
        ) {
            $self->{inlimit} = 0;
            $self->{buffer}  = "";
            return 0;
        }
        else {
            $self->{inlimit} = $rsize? ($self->{inlimit} - $rsize) : 0
        }
    }

    return 1;
}

##
##   Initialization
##   Usage: SELF->_init(STREAM_OUT, APPEND_FLAG)
##

sub _init
{
    my ($self, $outref, $s_append) = @_;

    $self->{l_rsize} = 0;
    $self->{l_wsize} = 0;
    $self->{l_match} = "";

    if (ref($outref) eq "SCALAR") {
        if ($s_append) {$$outref .= ""} else {$$outref = ""}
    }
}

##
##   Write data into output stream
##   Usage: BOOL = _store(STREAM_OUT, DATA_REF)
##

sub _store($$)
{
    my ($outref, $dataref) = @_;

    if (ref($outref) eq "SCALAR") {
        $$outref .= $$dataref; return 1;
    }
    elsif (ref($outref) eq "GLOB") {
        return 1 if print({$outref} $$dataref);
    }

    return 0;
}

1;

__END__

=head1 NAME

AZ::Splitter - Splitting and extracting data from file or from string, with loading into memory in parts

=head1 SYNOPSIS

  use AZ::Splitter;    # With flags importing into current namespace
  use AZ::Splitter (); # Access to flags from AZ:: namespace only

  $s = new AZ::Splitter(\*IN);
  $s = new AZ::Splitter(\*IN, $limitsize);
  $s = new AZ::Splitter(\*IN, $limitsize, $buffsize);
  $s = new AZ::Splitter(\*IN, $limitsize, $buffsize, s_2BUFF|s_UTFOK);
  $s = new AZ::Splitter(\$SCALAR, ...); # Same for scalar variables

  $s->read_to(\*OUT, $separator);
  $s->read_to(\*OUT, $separator, $maxsize);
  $s->read_to(\*OUT, $separator, $maxsize, s_APPEND|s_WEND|s_WCASE);
  $s->read_to(\*OUT, \@separators, ...) # Many separators
  $s->read_to(\$SCALAR, ...); # Same for scalar variables
  $s->read_to(undef, ...);    # Only rewind

  $s->read_some(\*OUT);
  $s->read_some(\*OUT, $maxsize);
  $s->read_some(\*OUT, $maxsize, s_APPEND);
  $s->read_some(\$SCALAR, ...); # Same for scalar variables
  $s->read_some(undef, ...);    # Only rewind

  $s->stat_rsize();
  $s->stat_wsize();
  $s->stat_match();
  $s->stat_error();

=head1 DESCRIPTION

This is object-oriented class for fast and simple splitting big files or
strings with using little memory. As the same can be used for searching
data in files, but the main fuction is parsing big volume data.

=head1 METHODS

=over 4

=item new(STREAM_IN, [SIZE_LIMIT, BUFF_SIZE, FLAGS])

The constructor method instantiates a new AZ::Splitter object.

STREAM_IN - is reference to file stream, opened for read, or reference
to string.

SIZE_LIMIT - limit size of input stream data in characters. If this argument
not defined or equal to -1, then all input stream data will be avaible for read.

BUFF_SIZE - size of buffer in characters. If this argument not defined
or equal to -1, then will be used default buffer size 32768 characters.

FLAGS - in this method avaible 2 modificators:

=over 4

s_2BUFF - use second buffer. Can really speed up search in case insensitive mode.

s_UTFOK - disable utf8 data check in UTF-8 mode. Use this flag if you really sure,
what your UTF-8 data is valid.

=back

=item read_to(STREAM_OUT, SEPARATORS, [MAX_SIZE, FLAGS])

This method reads all data from input stream before first found separator,
begining from current position. Returns true value if found separator or end of
stream expected at first time. Else, or at reading error, will returns false value.

STREAM_OUT - is reference to file stream, opened for write, or reference to
string. If other type of value (ex: undef), then data will be not stored.

SEPARATORS - is string-separator or reference to array with many separators.

MAX_SIZE - size in characters. Defines, how much maximum characters must be
stored into STREAM_OUT. If this argument not defined or equal to -1, then this
method will be trying to store all readed data.

FLAGS - in this method avaible 3 modificators:

=over 4

s_APPEND - append data to STREAM_OUT if STREAM_OUT is reference to string.

s_WCASE - search in case insencitive mode.

s_WEND - at the end of input stream alltimes returns the false value.

=back

=item read_some(STREAM_OUT, [MAX_SIZE, FLAGS])

This method method read fixed quantity of characters from input stream
begining from current position. Returns true value, if any characters was
readed or end of input stream not expected. Else, or at reading error,
will returns false value.

STREAM_OUT - same as in read_to() method.

MAX_SIZE - limit size in characters, how many is necessary to read. If this
argument not defined or equal to -1, then will be readed all avaible data
from input stream.

FLAGS - only one modificator avaible:

=over 4

s_APPEND - same as in read_to() method.

=back

=item stat_wsize()

Returns quantity of characters, what was readed at last using of read_to()
or read_some() methods. After using read_to() method this size not including
size of found separator.

=item stat_rsize()

Returns quantity of characters, what was successfully stored at last using
of read_to() or read_some() methods. This parameter can't be more then
stat_wsize() parameter.

=item stat_match()

Returns found separator at last use of read_to() method or empty string.

=item stat_error()

Returns error status. In any reading errors methods read_to() and read_some()
immediately stop any operations, returns false status and set up this paramerer
to true value.

=back

=head1 UTF-8 SUPPORT

Full supported when using perl version 5.8.0, or higher. Input stream,
separators, and result reference(if this is file stream), must be in UTF-8
mode. If input stream will have malformed UTF-8 data, then reading from
stream will be immediately stoped.

=head1 WARNINGS

Remember! This class using block reading and before destruct class-object,
you should work with input stream only through this class methods.

In UTF-8 mode search without case sensitive is very slowly.. It becouse
operation of changing case on UTF-8 data have slow speed.

Remember, at UTF-8 mode all sizes of this module containing
characters, not bytes!

All flags exists in AZ:: namespace and you can not import them to your
namespace if you don't want.

=head1 EXAMPLES

Reading configuration file:

  # Opening configuration file
  open(F, "<", "config.txt") or die $!;

  # Create new AZ::Splitter object
  # and associate with opened configuration file
  my $s = new AZ::Splitter(\*F);

  # Reading line per line in variable $line
  for (my $line; $s->read_to(\$line, "\r\n");)
  {
      # Do something with $line
  }

Find one of substrings in file without case sensetive:

  # Initialize array of strings
  my @strings=("word1", "word2", "phrase 1", "word3");

  # Open some file for reading
  open(F, "<", "file.txt") or die $!;

  # Create new AZ::Splitter object
  # and associate with opened file
  my $s = new AZ::Splitter(\*F);

  # Try to find, and print substring, when will found.
  my $result = $s->read_to(undef, \@strings, -1, s_WCASE|s_WEND)

  if ($result) {
      print "Found substring '".$s->stat_match()."'\n";
  }
  else {
      print "Nothing found..\n";
  }

=head1 AUTHOR

Andrian Zubko aka Ondr, E<lt>ondr@mail.ruE<gt>

=cut
