package AZ::Splitter;

use 5.005;
use strict;
use Exporter;

our @ISA    = qw(Exporter);
our @EXPORT = qw(s_APPEND s_WCASE s_WEND s_2BUFF s_UTFOK);

our $VERSION = '0.63';

#=================================================================================================#
#                                                                                                 #
#   Global variable(s) definition                                                                 #
#                                                                                                 #
#=================================================================================================#

our $_arg;

# System flags

our $_s_append = 0b00000001;
our $_s_wcase  = 0b00000010;
our $_s_wend   = 0b00000100;
our $_s_2buff  = 0b00001000;
our $_s_utfok  = 0b00010000;

#=================================================================================================#
#                                                                                                 #
#   Public methods and functions                                                                  #
#                                                                                                 #
#=================================================================================================#

# Functions-flags for export

sub s_APPEND () { $_s_append }
sub s_WCASE  () { $_s_wcase  }
sub s_WEND   () { $_s_wend   }
sub s_2BUFF  () { $_s_2buff  }
sub s_UTFOK  () { $_s_utfok  }

# Functions-flags in AZ:: namespace

sub AZ::s_APPEND () { $_s_append }
sub AZ::s_WCASE  () { $_s_wcase  }
sub AZ::s_WEND   () { $_s_wend   }
sub AZ::s_2BUFF  () { $_s_2buff  }
sub AZ::s_UTFOK  () { $_s_utfok  }

##########
# Public method. Constructor.
# Initializing new class-object with default parameters
sub new {
  _usage("OBJ = AZ::Splitter->new(INPUT[,LIMIT[,BUFFSIZE[,FLAGS]]])")
    unless( @_ >= 2 and @_ <= 5 );

  my $class    = shift;
  my $input    = shift;
  my $inLimit  = ( defined( $_arg = shift ) and $_arg >= 0 ) ? $_arg : 1e10;
  my $buffSize = ( defined( $_arg = shift ) and $_arg >= 0 ) ? $_arg : 32_768;
  my $flags    = shift || 0;

  my $self = {
               bufferA  => '',
               bufferB  => '',
               buffSize => $buffSize,
               inLimit  => $inLimit,
               inPos    => 0,
               input    => $input,
               lRSize   => 0,
               lWSize   => 0,
               lMatch   => '',
               status   => 1,
               s_2buff  => ( $_s_2buff & $flags ),
               s_utfok  => ( $_s_utfok & $flags )
  };
  return bless( $self, $class );
}

# Methods for access to statictics

sub stat_rsize {
  _usage("SIZE = OBJ->stat_rsize()") unless( @_ == 1 );
  return shift->{lRSize};
}

sub stat_wsize {
  _usage("SIZE = OBJ->stat_wsize()") unless( @_ == 1 );
  return shift->{lWSize};
}

sub stat_match {
  _usage("MATCH = OBJ->stat_match()") unless( @_ == 1 );
  return shift->{lMatch};
}

sub stat_error {
  _usage("BOOL = OBJ->stat_error()") unless( @_ == 1 );
  return defined( shift->{status} ) ? 0 : 1;
}

##########
# Public method read_to()
sub read_to {
  _usage("OBJ->read_to(OUTPUT,SEPARATOR[,LIMIT[,FLAGS]])")
    unless( @_ >= 3 and @_ <= 5 );

  my $self   = shift;
  my $output = shift;
  my $bound  = ( ref( $_arg = shift ) eq 'ARRAY' ) ? $_arg : [$_arg];
  my $limit  = ( defined( $_arg = shift ) and $_arg >= 0 ) ? $_arg : 1e10;
  my $flags  = shift || 0;
  my $maxB   = 0;
  my $minB   = 1e10;
  my $wcase  = $_s_wcase & $flags;
  my $error;
  my $rSize;

  # Preparing:
  #  - statistics variables definition by default values
  #  - initialize output stream, if this is SCALAR and initialization required
  @$self{qw(lRSize lWSize lMatch)} = ( (0) x 2, '' );
  $$output = ''
    if( UNIVERSAL::isa( $output, 'SCALAR' )
        and !( defined($$output) and ( $_s_append & $flags ) ) );
  #  - maximum and minimum size separators detection
  foreach(@$bound) {
    $maxB = length if( $maxB < length );
    $minB = length if( $minB > length );
  }

  #  - checking status and separators
  return( $self->{status} or 0 )
    unless( $self->{status} and $maxB );

  # Processing:
  while(1) {
    #  - searching
    if( length( $self->{bufferA} ) >= $minB ) {
      my $found = 1e10;
      my $buffer;
      $buffer = \( $self->{s_2buff} ? $self->{bufferB} : lc( $self->{bufferA} ) ) if $wcase;
      foreach(@$bound) {
        my $pos = $wcase ? index( $$buffer, lc ) : index( $self->{bufferA}, $_ );
        if( $pos != -1 and $pos < $found ) {
          $found = $pos;
          $self->{lMatch} = $_;
        }
      }
      if( $found < 1e10 ) {
        if( !$error and $self->{lRSize} < $limit ) {
          $rSize = $found;
          $rSize = $limit - $self->{lRSize} if( $rSize > $limit - $self->{lRSize} );
          $error = !$self->_write( $output, \( substr( $self->{bufferA}, 0, $rSize ) ) );
          $self->{lRSize} += $rSize unless $error;
        }
        $self->{lWSize} += $found;
        my $pSize = $found + length( $self->{lMatch} );
        substr( $self->{bufferA}, 0, $pSize, '' );
        substr( $self->{bufferB}, 0, $pSize, '' ) if $self->{s_2buff};
        return 1;
      }
    }
    #  - move part data to output stream
    if( length( $self->{bufferA} ) >= $maxB ) {
      my $pSize = length( $self->{bufferA} ) - ( $maxB - 1 );
      if( !$error and $self->{lRSize} < $limit ) {
        $rSize = $pSize;
        $rSize = $limit - $self->{lRSize} if( $rSize > $limit - $self->{lRSize} );
        $error = !$self->_write( $output, \( substr( $self->{bufferA}, 0, $rSize ) ) );
        $self->{lRSize} += $rSize unless $error;
      }
      $self->{lWSize} += $pSize;
      substr( $self->{bufferA}, 0, $pSize, '' );
      substr( $self->{bufferB}, 0, $pSize, '' ) if $self->{s_2buff};
    }
    #  - if limit then try to fill buffer
    #  - else move last data to output stream and finish
    if( $self->{inLimit} ) {
      return 0 unless $self->_fill_buffer();
    }
    else {
      if( length $self->{bufferA} ) {
        if( !$error and $self->{lRSize} < $limit ) {
          $rSize = length $self->{bufferA};
          $rSize = $limit - $self->{lRSize} if( $rSize > $limit - $self->{lRSize} );
          $error = !$self->_write( $output, \( substr( $self->{bufferA}, 0, $rSize ) ) );
          $self->{lRSize} += $rSize unless $error;
        }
        $self->{lWSize} += length $self->{bufferA};
        $self->{bufferA} = '';
        $self->{bufferB} = '' if $self->{s_2buff};
      }
      $self->{status} = 0;
      return( ( $_s_wend & $flags ) ? 0 : 1 );
    }
  }
}

##########
# Public method read_some()
sub read_some {
  _usage("OBJ->read_some(OUTPUT[,LIMIT[,FLAGS]])")
    unless( @_ >= 2 and @_ <= 4 );

  my $self   = shift;
  my $output = shift;
  my $limit  = ( defined( $_arg = shift ) and $_arg >= 0 ) ? $_arg : 1e10;
  my $flags  = shift || 0;
  my $rSize;
  my $error;

  # Preparing:
  #  - statistics variables definition by default values
  #  - initialize output stream, if this is SCALAR and initialization required
  @$self{qw(lRSize lWSize lMatch)} = ( (0) x 2, '' );
  $$output = ''
    if( UNIVERSAL::isa( $output, 'SCALAR' )
        and !( defined($$output) and ( $_s_append & $flags ) ) );
  #  - checking status
  return 0 unless $self->{status};

  # Processing:
  while( $self->{lWSize} < $limit ) {
    #  - trying to fill buffer
    unless( length $self->{bufferA} ) {
      return 0 unless $self->_fill_buffer();
    }
    #  - if buffer still empty then break cycle
    #  - else if not enouth data in buffer, then move all data from buffer to output stream
    #  - else move necessary of characters to output stream  and break cycle
    unless( length $self->{bufferA} ) {
      $self->{status} = 0;
      return( $self->{lWSize} ? 1 : 0 );
    }
    elsif( length( $self->{bufferA} ) <= $limit - $self->{lWSize} ) {
      $error = !$self->_write( $output, \( $self->{bufferA} ) ) unless $error;
      $self->{lRSize} += length( $self->{bufferA} ) unless $error;
      $self->{lWSize} += length( $self->{bufferA} );
      $self->{bufferA} = '';
      $self->{bufferB} = '' if $self->{s_2buff};
    }
    else {
      $rSize = $limit - $self->{lWSize};
      $error = !$self->_write( $output, \( substr( $self->{bufferA}, 0, $rSize ) ) ) unless $error;
      $self->{lRSize} += $rSize unless $error;
      $self->{lWSize} += $rSize;
      substr( $self->{bufferA}, 0, $rSize, '' );
      substr( $self->{bufferB}, 0, $rSize, '' ) if $self->{s_2buff};
      last;
    }
  }

  return 1;
}

#=================================================================================================#
#                                                                                                 #
#   Private methods and functions                                                                 #
#                                                                                                 #
#=================================================================================================#

##########
# Private method. Usage: BOOL = SELF->_fill_buffer()
# Attempts to fill buffer, returns boolean value
sub _fill_buffer {
  my $self = shift;
  return 1 unless $self->{inLimit}; # checking stream limit

  my $length;
  my $result;
  $length = $self->{buffSize};
  $length = $self->{inLimit} if( $length > $self->{inLimit} );
  $result = $self->_read( \( $self->{bufferA} ), $length );

  # Checking
  if(
      !defined($result)
      or (     $] >= 5.008001
           and !$self->{s_utfok}
           and $result
           and utf8::is_utf8( $self->{bufferA} )
           and !utf8::valid( $self->{bufferA} ) )
    )
  {
    @$self{qw(inLimit bufferA)} = ( 0, '' );
    return undef( $self->{status} ); # false on error
  }

  # Fixing stream limit and care about second buffer
  $self->{inLimit} = $result ? ( $self->{inLimit} - $result ) : 0;
  $self->{bufferB} = lc( $self->{bufferA} ) if $self->{s_2buff};
  return 1;
}

##########
# Private method. Usage: LENGTH = SELF->_read(STRREF,LENGTH)
# Reading data from input stream and appending to STRREF
sub _read {
  my $self   = shift;
  my $strref = shift;
  my $length = shift;
  my $result;

  # Checking type of stream
  if( UNIVERSAL::isa( $self->{input}, 'SCALAR' ) ) {
    $result = length( ${ $self->{input} } ) - $self->{inPos};
    $result = $length if $result > $length;
    $result = 0 if $result < 0;
    $$strref .= substr( ${ $self->{input} }, $self->{inPos}, $result );
    $self->{inPos} += $result;
  }
  elsif( UNIVERSAL::isa( $self->{input}, 'GLOB' ) ) {
    $result = read( $self->{input}, my $data, $length );
    $$strref .= $data;
  }

  return $result;
}

##########
# Private method. Usage: BOOL = SELF->_write(OUTPUT,STRREF)
# Writing data into output stream
sub _write {
  my $self   = shift;
  my $output = shift;
  my $strref = shift;
  my $result;

  # Checking type of stream
  if( UNIVERSAL::isa( $output, 'SCALAR' ) ) {
    $$output .= $$strref;
    $result = 1; # alltimes true
  }
  elsif( UNIVERSAL::isa( $output, 'GLOB' ) ) {
    $result = print( {$output} $$strref );
  }

  return $result;
}

##########
# Private function. Usage: _usage(HOWTO)
# Handling 'Usage' messages
sub _usage($) { require Carp; Carp::croak( 'Usage: ' . shift ) }

1;
