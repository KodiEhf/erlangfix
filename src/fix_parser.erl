-module(fix_parser).
-include("fix.hrl").
-compile(export_all).

%% Data = <<"8=FIX.4.2|9=178|35=8|49=PHLX|56=PERS|52=20071123-05:30:00.000|11=ATOMNOCCC9990900|20=3|150=E|39=E|55=MSFT|167=CS|54=1|38=15|40=2|44=15|58=PHLX EQUITY TESTING|59=0|47=C|32=0|31=0|151=15|14=0|6=0|10=128|">>.

%%<<"8=FIX.4.2|9=65|35=A|49=SERVER|56=CLIENT|34=177|52=20090107-18:15:16|98=0|108=30|10=062|">>

%% We need a mapping of integers to tag-names
%% We know that a message ends with "10=xyz |"
%% Parse should return a list of "parsed messages" and a binary rest
%% BodyLength is always the second field in the message,
%% length refers to the message length up to checksum field

p(<<_Data/binary>>, 0) ->
    ok;
p(<<Data/binary>>, Count) ->
    parse(Data),
    p(Data, Count-1).

parse(<<Data/binary>>) ->
    %% if something goes wrong here - the message is incomplete
    try 
	%% Retreive the first field from the message, which is 'BeginString'
	[First, Rest] = binary:split(Data, [<<?SOH>>], []),
	{'BeginString', _Value} = field_parse(First),
	%% Retreive the second field from the message, which is 'BodyLength'
	[Second, _Message] = binary:split(Rest, [<<?SOH>>], []),
	{'BodyLength', BodyLength} = field_parse(Second),
	%% Calculate the total length of the message, minus the 'CheckSum' field
	MessageLength = BodyLength + length(binary_to_list(First)) + length(binary_to_list(Second)) + 2, %%2 x SOH
        TotalMessageLength = MessageLength + 6
	%io:format("TotalMessageLength = ~p~n", [TotalMessageLength]),
	%% The rest of the message we have to parse
	%%MessageBody = binary:part(Message, 0, Length),
	%% Validate the checksum
	%%{'CheckSum', Sum} = field_parse(binary:part(Message, Length, 6)), %%the length of the checksum field is always 6
	%io:format("Sum = ~p~n", [Sum]),
	%<<CheckSumData:MessageLength/binary, _/binary>> = <<Data/binary>>,
	%CheckSum = check_sum(CheckSumData)
	%% Find the message Type
	%<<Current:TotalMessageLength/binary, _Other/binary>> = <<Data/binary>>,
	%FieldList = binary:split(Current, [<<?SOH>>], [global]),
	%ParsedMessage = lists:map(fun(Elem) -> field_parse(Elem) end, FieldList)
	%binary:split(Current, [<<?SOH>>], [global])
    catch
	error:Err -> io:format(Err)
    end.
    
field_parse(<<"8=",Value/binary>>) ->
    {'BeginString', Value};

field_parse(<<"9=",Value/binary>>) ->
    {'BodyLength', to_int(Value)};

field_parse(<<"10=",Value/binary>>) ->
    {'CheckSum', to_int(Value)};

field_parse(<<F:1/binary, "=",Value/binary>>) ->
    {F, Value};

field_parse(<<F:2/binary, "=",Value/binary>>) ->
    {F, Value};

field_parse(<<F:3/binary, "=", Value/binary>>) ->
    {F, Value};

field_parse(Other) ->
    {unknown, Other}.

check_sum(<<Message/binary>>) ->
    List = binary_to_list(Message),
    lists:sum(List) rem 256.

%% field_parse([<<"35=",Value/binary>> | Rest], [Current | ResultSet]) when Rest =/= [] ->
%%     NewCurrent = [{'MsgType', to_int(Value)} | Current],
%%     field_parse(Rest, [NewCurrent | ResultSet]);

%% field_parse([<<"10=",Value/binary>> | Rest], [Current | ResultSet]) when Rest =/= [] ->
%%     NewCurrent = lists:reverse([{'BodyLength', to_int(Value)} | Current]),
%%     field_parse(Rest, [NewCurrent|ResultSet]);

%% %for unknown and inclomplete fields
%% field_parse([<<_Other/binary>> | Rest], [Current | ResultSet]) ->
%%     {lists:reverse(Current), ResultSet}.

to_int(Binary) ->
    list_to_integer(binary_to_list(Binary)).
