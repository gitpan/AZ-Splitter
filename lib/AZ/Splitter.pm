package AZ::Splitter;

use 5.005;
use strict;
use Exporter;
use autouse Carp => qw(croak);

our @ISA     = qw(Exporter);
our @EXPORT  = qw(s_APPEND s_WCASE s_WEND s_2BUFF s_UTFOK);

our $VERSION = q(0.60);

##
##   Define system flags
##

our $_s_append = 0b00000001;
our $_s_wcase  = 0b00000010;
our $_s_wend   = 0b00000100;
our $_s_2buff  = 0b00001000;
our $_s_utfok  = 0b00010000;

##============================================================================##
##                                                                            ##
##   Public methods and functions                                             ##
##                                                                            ##
##============================================================================##

##
##   Functions-flags for export
##

sub s_APPEND () { $_s_append }
sub s_WCASE  () { $_s_wcase  }
sub s_WEND   () { $_s_wend   }
sub s_2BUFF  () { $_s_2buff  }
sub s_UTFOK  () { $_s_utfok  }

##
##   Functions-flags in AZ:: namespace
##

sub AZ::s_APPEND () { $_s_append }
sub AZ::s_WCASE  () { $_s_wcase  }
sub AZ::s_WEND   () { $_s_wend   }
sub AZ::s_2BUFF  () { $_s_2buff  }
sub AZ::s_UTFOK  () { $_s_utfok  }

##
##   Constructor
##

sub new
{
    unless(@_ >= 2 and @_ <= 5) {
      croak("Usage: OBJECT = AZ::Splitter->new(STREAM_IN,[SIZE_LIMIT,BUFF_SIZE,FLAGS])");
    }
    my($class,$stIN,$stLimit,$buffSize,$F) = (@_,(0)x3);
    my($self);

    # Checking/correcting arguments
    $stLimit  = 1e10    if @_ < 3 or $stLimit  < 0;
    $buffSize = 32*1024 if @_ < 4 or $buffSize < 0;

    $self = {
          buffer   => q(),
          buffer2  => q(),
          buffSize => $buffSize,
          stLimit  => $stLimit,
          stLPos   => 0,
          stIN     => $stIN,
          lMatch   => q(),
          lRSize   => 0,
          lWSize   => 0,
          s_2buff  => $_s_2buff &$F,
          s_utfok  => $_s_utfok &$F,
          status   => 1,
    };
    return bless($self,$class);
}

##
##   Statistics
##

sub stat_rsize {
    croak("Usage: SIZE = OBJECT->stat_rsize()") unless(@_ == 1);
    return shift->{lRSize};
}

sub stat_wsize {
    croak("Usage: SIZE = OBJECT->stat_wsize()") unless(@_ == 1);
    return shift->{lWSize};
}

sub stat_match {
    croak("Usage: MATCH = OBJECT->stat_match()") unless(@_ == 1);
    return shift->{lMatch};
}

sub stat_error {
    croak("Usage: BOOL = OBJECT->stat_error()") unless(@_ == 1);
    return defined(shift->{status})? 0 : 1;
}

##
##   Method read_to()
##

sub read_to
{
    unless(@_ >= 3 and @_ <= 5) {
      croak("Usage: OBJECT->read_to(STREAM_OUT,SEPARATORS,[MAX_SIZE,FLAGS])");
    }
    my($self,$stOUT,$sep,$mSize,$F) = (@_,(0)x2);
    my($bSize,$cep) = 0;

      # Preparing
      $self->{lRSize} = 0;
      $self->{lWSize} = 0;
      $self->{lMatch} = q();
      $$stOUT = q() if ref($stOUT) eq q(SCALAR)
        and not (defined($$stOUT) and ($_s_append &$F));
    return 0 unless $self->{status};

    # Checking/correcting arguments
    $mSize = 1e10 if @_ < 4 or $mSize < 0;
    $sep = [$sep] if ref($sep) ne q(ARRAY);
    for(@$sep)
    {
        $bSize = length if $bSize < length;
        push(@$cep,lc)  if $_s_wcase &$F;
    }
    unless($bSize) {
        return($self->{status} or 0);
    }

    {
        my($error,$buffer,$rSize,$pSize);
        my($fPos,$fInd,$pos,$curSep);

        # Reading data from stream
        # through buffer before first found separator
        for(;;)
        {
            # If buffer have enough size,
            # then trying to searching separator(s) in them
            if (length($self->{buffer}) >= $bSize)
            {
                # Preparing to search
                $fPos = 1e10;
                $cep?
                  ($buffer = \($self->{s_2buff}? $self->{buffer2} : lc($self->{buffer})))
                 :($buffer = \($self->{buffer}));

                # Searching first separator in buffer
                for(0..(@$sep-1))
                {
                    $pos = index($$buffer,($cep? $cep->[$_] :$sep->[$_]));
                  next unless(
                      $pos > -1 and $pos < $fPos);
                    ($fInd,$fPos) = ($_,$pos);
                }

                # If found, then move part
                # into result stream and break with status code 0
                if ($fPos < 1e10) {
                  if (not $error and $self->{lRSize} < $mSize)
                  {
                      $rSize = $fPos;
                      $rSize = $mSize - $self->{lRSize} if($rSize > $mSize - $self->{lRSize});
                      $error = !_store($stOUT,\(substr($self->{buffer},0,$rSize)));
                      $self->{lRSize} += $rSize unless $error;
                  }
                  $self->{lWSize} += $fPos;
                  $self->{lMatch}  = $sep->[$fInd];
                  $pSize = $fPos + length($sep->[$fInd]);
                  substr($self->{buffer},0,$pSize,q());
                  substr($self->{buffer2},0,$pSize,q()) if $self->{s_2buff};
                  return 1;
                }
                # Else move part of buffer to result stream without last
                # ($bSize - 1) characters at right side of buffer
                else
                {
                  $pSize = length($self->{buffer}) - ($bSize - 1);
                  if (not $error and $self->{lRSize} < $mSize)
                  {
                      $rSize = $pSize;
                      $rSize = $mSize - $self->{lRSize} if($rSize > $mSize - $self->{lRSize});
                      $error = !_store($stOUT,\(substr($self->{buffer},0,$rSize)));
                      $self->{lRSize} += $rSize unless $error;
                  }
                  $self->{lWSize} += $pSize;
                  substr($self->{buffer},0,$pSize,q());
                  substr($self->{buffer2},0,$pSize,q()) if $self->{s_2buff};
                }
            }

            # Trying to fill buffer
            if ($self->{stLimit})
            {
              return(undef($self->{status}) or 0)
                  unless $self->_fill_buffer();
                $self->{buffer2} = lc($self->{buffer}) if $self->{s_2buff};
            }
            # Else, if buffer empty and nothing more data in stream
            # then moving last data to output stream and break cycle
            else
            {
                if (length($self->{buffer})) {
                  if (not $error and $self->{lRSize} < $mSize)
                  {
                      $rSize = length($self->{buffer});
                      $rSize = $mSize - $self->{lRSize} if($rSize > $mSize - $self->{lRSize});
                      $error = !_store($stOUT,\(substr($self->{buffer},0,$rSize)));
                      $self->{lRSize} += $rSize unless $error;
                  }
                  $self->{lWSize} += length($self->{buffer});
                  $self->{buffer}  = q();
                  $self->{buffer2} = q() if $self->{s_2buff};
                }
                $self->{status} = 0;
                return(($_s_wend &$F)? 0 : 1);
            }
        }
    }
}

##
##   Method read_some()
##

sub read_some
{
    unless(@_ >= 2 and @_ <= 4) {
      croak("Usage: OBJECT->read_some(STREAM_OUT,[MAX_SIZE,FLAGS])");
    }
    my($self,$stOUT,$mSize,$F) = (@_,(0)x2);
    my($error,$rSize);

      # Preparing
      $self->{lRSize} = 0;
      $self->{lWSize} = 0;
      $self->{lMatch} = q();
      $$stOUT = q() if ref($stOUT) eq q(SCALAR)
        and not (defined($$stOUT) and ($_s_append &$F));
    return 0 unless $self->{status};

    # Checking/correcting arguments
    $mSize = 1e10 if @_ < 3 or $mSize < 0;

    # Reading data from input stream through buffer
    while($self->{lWSize} < $mSize)
    {
        # Trying to fill buffer
        if (not length($self->{buffer}))
        {
          return(undef($self->{status}) or 0)
              unless $self->_fill_buffer();
            $self->{buffer2} = lc($self->{buffer}) if $self->{s_2buff};
        }

        # Well,
        # if buffer still empty then break cycle
        if (not length($self->{buffer}))
        {
            $self->{status} = 0;
            return($self->{lWSize}? 1 : 0);
        }
        # If not enouth data in buffer,
        # then move all data from buffer to output stream
        elsif (length($self->{buffer}) <= $mSize - $self->{lWSize})
        {
            unless($error) {
              $error = !_store($stOUT,\($self->{buffer}));
            }
            $self->{lRSize} += length($self->{buffer}) unless $error;
            $self->{lWSize} += length($self->{buffer});
            $self->{buffer}  = q();
            $self->{buffer2} = q() if $self->{s_2buff};
        }
        # Move necessary quantity of characters
        # from buffer to output stream and break cycle
        else
        {
            $rSize = $mSize - $self->{lWSize};
            unless($error) {
              $error = !_store($stOUT,\(substr($self->{buffer},0,$rSize)));
            }
            $self->{lRSize} += $rSize unless $error;
            $self->{lWSize} += $rSize;
            substr($self->{buffer},0,$rSize,q());
            substr($self->{buffer2},0,$rSize,q()) if $self->{s_2buff};
            return 1;
        }
    }

    # Returning true if all ok
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
    my($self,$rSize,$buffSize) = shift;
    my($tmp);

    # Limit checking
    return 1 unless $self->{stLimit};

    # Calculate buffer size
    $buffSize = $self->{buffSize};
    $buffSize = $self->{stLimit} if $buffSize > $self->{stLimit};

    # Reading part of data from input stream,
    # depending by type of stream reference
    if (ref($self->{stIN}) eq q(SCALAR))
    {
        # Calculate length of characters
        $rSize = length(${$self->{stIN}}) - $self->{stLPos};
        $rSize = $buffSize if $rSize > $buffSize;
        $rSize = 0         if $rSize < 0;

        # Get data from string by reference
        $self->{buffer} .= substr(${$self->{stIN}},$self->{stLPos},$rSize);
        $self->{stLPos} += $rSize;
    }
    elsif (ref($self->{stIN}) eq q(GLOB))
    {
        # Get data from file stream
        $rSize = read($self->{stIN},$tmp,$buffSize);
        $self->{buffer} .= $tmp;
    }

    # If was'nt errors, then correcting size
    # of stream limit. Else erasing buffer and breaking with zero status
    if (not defined($rSize)
    or
    $] >= 5.008001 and not $self->{s_utfok} and $rSize
    and utf8::is_utf8($self->{buffer}) and not utf8::valid($self->{buffer}))
    {
        $self->{stLimit} = 0;
        $self->{buffer}  = q();
      return 0;
    }
    else {
        $self->{stLimit} = $rSize? ($self->{stLimit}-$rSize) : 0;
    }

    # Returning true value if all ok
    return 1;
}

##
##   Storing data. Returns boolean value
##   Usage: BOOL = _store(STREAM_OUT,DATA_REF)
##

sub _store($$)
{
    my($stOUT,$dataRef) = @_;

    # Writing data into output stream
    # depending by type of stream reference
    if (ref($stOUT) eq q(SCALAR)) {
        $$stOUT .= $$dataRef;
      return 1;
    }
    if (ref($stOUT) eq q(GLOB)) {
      return 1
        if print({$stOUT} $$dataRef);
    }
    return 0;
}

1;
