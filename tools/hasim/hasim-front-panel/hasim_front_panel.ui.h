/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename functions or slots use
** Qt Designer which will update this file, preserving your code. Create an
** init() function in place of a constructor, and a destroy() function in
** place of a destructor.
*****************************************************************************/

void hfpanel::init()
{
    $scanning = 0;
    
    $ledArray[0] = led0_off;
    $ledArray[1] = led1_off;
    $ledArray[2] = led2_off;
    $ledArray[3] = led3_off;
    $ledArray[4] = led4_off;
    $ledArray[5] = led5_off;
    $ledArray[6] = led6_off;
    $ledArray[7] = led7_off;

    $switchArray[0] = switch0;
    $switchArray[1] = switch1;
    $switchArray[2] = switch2;
    $switchArray[3] = switch3;
    $switchArray[4] = switch4;
    $switchArray[5] = switch5;
    $switchArray[6] = switch6;
    $switchArray[7] = switch7;
    $switchArray[8] = switch8;

    my $string_of_32_zeroes = "00000000000000000000000000000000";
    @switchCache = split(//, $string_of_32_zeroes);

    # disable all input controls except Start button
    groupSwitches->setEnabled(0);
    groupButtons->setEnabled(0);
    groupLEDs->setEnabled(0);

    $| = 1;
}

void hfpanel::startButton_clicked()
{
    my $timeout = 0.001;
    my $a = Qt::app();
    
    # sanity check
    if ($scanning eq 1)
    {
        die "invalid state: 1\n";
    }
        
    # enable dialog controls... do we need an atomic
    # condition variable here?
    $scanning = 1;

    startButton->setEnabled(0);
    groupSwitches->setEnabled(1);
    groupButtons->setEnabled(1);
    groupLEDs->setEnabled(1);
        
    # start scanning inputs
    my $sel = new IO::Select;
    $sel->add(\*STDIN);

    # flush all junk from STDIN
    # while ($sel->can_read($timeout))
    # {
    #    my $junk;
    #    read(STDIN, $junk, 1);
    # }
        
    # enter scan loop
    while ($scanning eq 1)
    {
        # scan for events
        if ($sel->can_read($timeout))
        {
            # incoming!
            my $data;
            read(STDIN, $data, 32);

            # update LED state: be careful about endian-ness!
            for (my $led = 0; $led < 8; $led = $led + 1)
            {
                # in our dialog box, the off-state LED is on top,
                # so to switch "on" the LED, we need to hide the
                # off-state pixmap, and to switch it "off", we need
                # to reveal the off-state pixmap
                if (substr($data, $led, 1) eq '0')
                {
                    $ledArray[$led]->show();
                }
                else
                {
                    $ledArray[$led]->hide();
                }
            }
        }
            
        # ugh, this is really ugly: manually ask
        # application to process events so that event
        # loop is alive. It is theoretically possible
        # for more than one instance of this loop to
        # be running simultaneously, but it is extremely
        # unlikely, and even if this happens, there
        # aren't any critical race conditions (I think).
          
        $a->processEvents();
    }
        
    # we've come out of the scan loop, perform cleanup
}

void hfpanel::buttonOk_clicked()
{
    # check if scan loop is running
    if ($scanning eq 0)
    {
        # nothing to do, simply exit
    }
    else
    {
        # set flag to come out of scan loop
        $scanning = 0;
    }

    # note: accept() will be called after this method ends,
    # which will cause the app to terminate
}



void hfpanel::sendSwitchMessage( int, int )
{
    # construct string and print it out
    $switchCache[$_[0]] = $_[1];

    print STDOUT @switchCache;

    #print STDOUT "$_[0] $_[1]\n";
}



void hfpanel::switch0_valueChanged( int )
{
    sendSwitchMessage(0, $_[0]);
}

void hfpanel::switch1_valueChanged( int )
{
    sendSwitchMessage(1, $_[0]);
}

void hfpanel::switch2_valueChanged( int )
{
    sendSwitchMessage(2, $_[0]);
}

void hfpanel::switch3_valueChanged( int )
{
    sendSwitchMessage(3, $_[0]);
}



void hfpanel::switch4_released()
{
    sendSwitchMessage(4, 0);
}

void hfpanel::switch4_pressed()
{
    sendSwitchMessage(4, 1);
}

void hfpanel::switch5_released()
{
    sendSwitchMessage(5, 0);
}

void hfpanel::switch5_pressed()
{
    sendSwitchMessage(5, 1);
}

void hfpanel::switch6_released()
{
    sendSwitchMessage(6, 0);
}

void hfpanel::switch6_pressed()
{
    sendSwitchMessage(6, 1);
}

void hfpanel::switch7_released()
{
    sendSwitchMessage(7, 0);
}

void hfpanel::switch7_pressed()
{
    sendSwitchMessage(7, 1);
}

void hfpanel::switch8_released()
{
    sendSwitchMessage(8, 0);
}

void hfpanel::switch8_pressed()
{
    sendSwitchMessage(8, 1);
}
