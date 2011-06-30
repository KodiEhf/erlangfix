-module(fix_framer).
-include("fix.hrl").
-export([frame/1]).

frame(Data) ->
    frame(Data, []).

frame(<<Data/binary>>, Res) ->
    try
	[First, Rest] = binary:split(Data, [<<?SOH>>], []),
	{'BeginString', _Value} = fix_parser:parse_field(First),
	[Second, _Message] = binary:split(Rest, [<<?SOH>>], []),
	{'BodyLength', BodyLength} = fix_parser:parse_field(Second),
	MessageLength = BodyLength + length(binary_to_list(First)) + length(binary_to_list(Second)) + 2,
	TotalMessageLength = MessageLength + 7,
	<<CurrentMessage:TotalMessageLength/binary, RestOfData/binary>> = <<Data/binary>>,
	Message = fix_parser:parse(CurrentMessage, TotalMessageLength),
	frame(RestOfData, Res++[Message])
    catch
	error: _Err -> {ok, Res, Data}
    end.
