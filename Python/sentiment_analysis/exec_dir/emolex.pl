#!/usr/bin/perl -w
use strict;

my $arg1; #-t ou -f
my $arg2; #tweet ou arquivo
my $c = 0;
our $text;

foreach my $argnum (0 .. $#ARGV)
{
    if($c == 0)
    {
         $arg1 = $ARGV[$argnum];
         $c+=1;
    }
    elsif($c == 1)
    {
        $arg2 = $ARGV[$argnum];
        $c+=1;
    }
}

open(IN, "./corpus.txt") or die "could not open corpus.txt\n";

our %NRC = ();

while (my $line = <IN>) 
{
    chomp $line;
    
    if( $line =~ /(\S+) (\S+) (\S+) (\S+) (\S+) (\S+) (\S+) (\S+) (\S+) (\S+) (\S+)/)
	{
        my $word = lc $1;
        
        my @array = split(":", $2);
        my $positive = $array[1];
        
        @array = split(":", $3);
        my $negative = $array[1];
        
        @array = split(":", $4);
        my $anger = $array[1];
        
        @array = split(":", $5);
        my $anticipation = $array[1];
        
        @array = split(":", $6);
        my $disgust = $array[1];
        
        @array = split(":", $7);
        my $fear = $array[1];
        
        @array = split(":", $8);
        my $joy = $array[1];
        
        @array = split(":", $9);
        my $sadness = $array[1];
        
        @array = split(":", $10);
        my $surprise = $array[1];
        
        @array = split(":", $11);
        my $trust = $array[1];

        $NRC{$word}{"positive"} = $positive;
        $NRC{$word}{"negative"} = $negative;
        $NRC{$word}{"anger"} = $anger;
        $NRC{$word}{"anticipation"} = $anticipation;
        $NRC{$word}{"disgust"} = $disgust;
        $NRC{$word}{"fear"} = $fear;
        $NRC{$word}{"joy"} = $joy;
        $NRC{$word}{"sadness"} = $sadness;
        $NRC{$word}{"surprise"} = $surprise;
        $NRC{$word}{"trust"} = $trust;
	}
}

if($arg1 eq "-t")
{
    $text = $arg2;
    &trataTweet;
 }
elsif($arg1 eq "-f")
{
    &abreArqs;
}

sub trataTweet
{
    my @words = split(" ", $text);
        
        my $pos;
        my $neg;
        my $ang; 
        my $anti;
        my $disg;
        my $fea;
        my $jo;
        my $sad;
        my $sur;
        my $trus;
        
        my $pos_total = 0;
        my $neg_total = 0;
        my $neut_total = 0;
        
        foreach(@words)
        {
            if(exists $NRC{$_})
            {
		my $w = $_;
                $pos = $NRC{$w}{'positive'};
                $neg = $NRC{$w}{'negative'};
                $ang = $NRC{$w}{'anger'}; 
                $anti = $NRC{$w}{'anticipation'};
                $disg = $NRC{$w}{'disgust'};
                $fea = $NRC{$w}{'fear'};
                $jo = $NRC{$w}{'joy'};
                $sad = $NRC{$w}{'sadness'};
                $sur = $NRC{$w}{'surprise'};
                $trus = $NRC{$w}{'trust'};
                
                $pos_total += $pos;
                
                $neg_total += $neg;
                # Matheus Fix: Now we based just int the positive and negative sentiments
                # $pos_total += $jo;
                # $pos_total += $trus;
                
                # $neg_total += $ang;
                # $neg_total += $disg;
                # $neg_total += $fea;
                # $neg_total += $sad;
                
                # $neut_total += $sur;
                # $neut_total += $anti;
            }
        }


    # Edited here (and below) to output both raw and rounded answers
        if($pos_total > $neg_total && $pos_total > $neut_total)
        {
            print "$pos_total\t1\n";
        }
        
        elsif($neg_total > $pos_total && $neg_total > $neut_total)
        {
            print "\-$neg_total\t-1\n";
        }
        
        else
        {
            print "0\t0\n";
        }
}

sub abreArqs
{
    open(IN2, "$arg2") or die "could not open $arg2\n";

    while (my $line = <IN2>) 
    {
        chomp $line;
        
        my @words = split(" ", $line);
        
        my $pos;
        my $neg;
        my $ang; 
        my $anti;
        my $disg;
        my $fea;
        my $jo;
        my $sad;
        my $sur;
        my $trus;
        
        my $pos_total = 0;
        my $neg_total = 0;
        my $neut_total = 0;
        
        foreach(@words)
        {
            if(exists $NRC{$_})
            {
                $pos = $NRC{$_}{'positive'};
                $neg = $NRC{$_}{'negative'};
                $ang = $NRC{$_}{'anger'}; 
                $anti = $NRC{$_}{'anticipation'};
                $disg = $NRC{$_}{'disgust'};
                $fea = $NRC{$_}{'fear'};
                $jo = $NRC{$_}{'joy'};
                $sad = $NRC{$_}{'sadness'};
                $sur = $NRC{$_}{'surprise'};
                $trus = $NRC{$_}{'trust'};
                
                $pos_total += $pos;
                $pos_total += $jo;
                $pos_total += $trus;
                
                $neg_total += $neg;
                $neg_total += $ang;
                $neg_total += $disg;
                $neg_total += $fea;
                $neg_total += $sad;
                
                $neut_total += $sur;
                $neut_total += $anti;
            }
        }
    # Edited here (and above) to output both raw and rounded answers    
        if($pos_total > $neg_total && $pos_total > $neut_total)
        {
            print "$pos_total\t1\n";
        }
        
        elsif($neg_total > $pos_total && $neg_total > $neut_total)
        {
            print "\-$neg_total\t-1\n";
        }
        
        else
        {
            print "0\t0\n";
        }
        
    }
}

close IN;
close IN2;

