use Irssi;
use JSON::PP qw( decode_json );
use strict;

# file format, given by example:
# {
#     "examplenet": {
#         "real_name": "Real Name",
#         "user_name": "user_name",
#         "nick": "nick",
#         "password": "<password>",
#         "server": "irc.example.net",
#         "port": "6697",
#         "ssl": true
#         "channels": [
#             "#example1",
#             "#example2"
#         ],
#     }
# }

my $config_file = glob('~/.irc.private.json');

if (! -e $config_file) {
    print "WARNING: config file $config_file does not exist";
    return;
}

my $config_str = do {
    open(my $config_fh, '<:encoding(UTF-8)', $config_file)
        or die("ERROR: can't open config file $config_file: $!");
    local $/ = undef;
    <$config_fh>
};

my $config = decode_json($config_str);
foreach my $network (keys %{$config}) {
    my $value = $config->{$network};

    # network
    my $real_name = $value->{'real_name'};
    my $user_name = $value->{'user_name'};
    my $nick = $value->{'nick'};
    my $password = $value->{'password'};
    Irssi::command("network add -user $user_name -realname \"$real_name\" " .
        "-nick $nick -autosendcmd \"msg nickserv identify $password; wait 5000\" $network");

    # server
    my $server = $value->{'server'};
    my $port = $value->{'port'};
    my $ssl = $value->{'ssl'};
    my $ssl_args = $ssl ? '-ssl -ssl_verify' : '';
    Irssi::command("server add -auto -network $network $ssl_args $server $port");

    # channels
    my $channels = $value->{'channels'};
    foreach my $channel (@$channels) {
        Irssi::command("channel add -auto $channel $network");
    }

    # connect
    Irssi::command("connect $network");
}
