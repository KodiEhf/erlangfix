-module(fix_code_gen).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

main() ->
    {#xmlElement{name=_Name, content=Content}, _} = xmerl_scan:file("FIX42.xml"),
    io:format("conent.length is ~p~n", [length(Content)]),
    %io:format("conent is ~p~n", [Content]).
    process_content(Content, 0).

%processes all child nodes of the 'fields' node
%process_fields should return 2 strings,
%one for parsing fields and the other for parsing enums 
process_content([#xmlElement{name=fields, content=Content, attributes=Attributes} | Rest], Level) ->
    lists:map(fun(C) -> process_field(C) end, Content);

process_content([#xmlElement{name=Name, content=Content, attributes=Attributes} | Rest], Level) ->
    %io:format("conent is xmlElement at level ~p, name is ~p~n", [Level, Name]),
    process_attribute(Attributes),
    process_content(Content, Level+1),
    process_content(Rest, Level);

process_content([#xmlText{value=Value} | Rest], Level) -> 
    %io:format("conent is xmlText, Value is ~p~n", [Value]), 
    process_content(Rest, Level);
process_content([], Level) -> void;
process_content(Other, Level) ->  io:format("conent is ??  ~p~n", [Other]).

%%%===================================================================
%%% Handles the parsing of all fields and enum values
%%%===================================================================
process_field(Element = #xmlElement{name=field, content=Content, attributes=Attributes}) ->
    generate_field_parse(Element);
process_field(_) -> void.

generate_field_parse(#xmlElement{name=field, content=Content, attributes=Attributes}) ->
    [#xmlAttribute{value=Number, name=number}] = [ X || X <- Attributes, X#xmlAttribute.name=:=number ],
    [#xmlAttribute{value=Name, name=name}] = [ Y || Y <- Attributes, Y#xmlAttribute.name=:=name ],
    [#xmlAttribute{value=Type, name=type}] = [ Z || Z <- Attributes, Z#xmlAttribute.name=:=type ],

    io:format("field_parse(<<\"~s=\", Value/binary>>) -> ~n", [Number]),
    case length(Content) > 0 of
	true -> io:format("Val = case Value of~n"),  
		lists:map(fun(Elem) -> generate_enum_parse(Elem) end, Content),
		io:format("_ -> unknown~n"),
		io:format("end,~n"),
		io:format("{'~s', Val};~n", [Name]);
	false ->
	    case Type of
		"LENGTH" -> io:format("{'~s', to_int(Value)};~n", [Name]);
		"INT" -> io:format("{'~s', to_int(Value)};~n", [Name]);
		"SEQNUM" -> io:format("{'~s', to_int(Value)};~n", [Name]);
		"PRICE" -> io:format("{'~s', to_float(Value)};~n", [Name]);
		"CURRENCY" -> io:format("{'~s', to_float(Value)};~n", [Name]);
		"QTY" -> io:format("{'~s', to_float(Value)};~n", [Name]);
		_ -> io:format("{'~s', Value};~n", [Name])
	    end
    end.
   		      
generate_enum_parse(#xmlElement{name=Name, content=Content, attributes=Attributes}) ->
    [A1] = [ X || X <- Attributes, X#xmlAttribute.name=:=enum ],
    [A2] = [ Y || Y <- Attributes, Y#xmlAttribute.name=:=description ],
    io:format("<<\"~s\">> -> '~s'; ~n", [A1#xmlAttribute.value, A2#xmlAttribute.value]);
generate_enum_parse(_) -> "".


    
    

process_attribute([]) ->
    void;
process_attribute([#xmlAttribute{value=Value, name=Name} | Rest]) ->
    %io:format("xmlAttribute name is ~p value is ~s~n", [Name, Value]),
    process_attribute(Rest).

remove_quote(Value) ->
    NewValue = binary_to_list(iolist_to_binary(Value)),
    lists:filter(fun(V) -> io:format("V = ~p~n",[V]), V =/= $" end, NewValue).


