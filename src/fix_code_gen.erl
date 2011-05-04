-module(fix_code_gen).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

main() ->
    {#xmlElement{name=_Name, content=Content}, _} = xmerl_scan:file("FIX42.xml"),
    {ok, FileDescriptor} = file:open("p.erl", [write]), 
    process_content(Content, FileDescriptor),
    file:close(FileDescriptor).

%processes all child nodes of the 'fields' node
%process_fields should return 2 strings,
%one for parsing fields and the other for parsing enums 
process_content([#xmlElement{name=fields, content=Content, attributes=_Attributes} | Rest], FD) ->
    lists:map(fun(C) -> process_field(C, FD) end, Content),
    process_content(Rest, FD);

process_content([#xmlElement{name=header, content=_Content, attributes=_Attributes} = Element | Rest], FD) ->
    %%TODO create validation function for header, body, and trailer
    %%Repeating Groups
    %%DataType validation
    %%Parse should return {ok, Message} or {error, Reason},
    generate_validator(Element, FD),
    process_content(Rest, FD);

process_content([#xmlElement{name=trailer, content=_Content, attributes=_Attributes} = Element | Rest], FD) ->
    generate_validator(Element, FD),
    process_content(Rest, FD);

process_content([#xmlElement{name=messages, content=Content, attributes=_Attributes} | Rest], FD) ->
    lists:map(fun(C) -> generate_validator(C, FD) end, Content),
    process_content(Rest, FD);

process_content([#xmlElement{name=_Other, content=Content, attributes=_Attributes} | Rest], FD) ->
    process_content(Content, FD),
    process_content(Rest, FD);

process_content([#xmlText{value=_Value} | Rest], FD) -> 
    process_content(Rest, FD);
process_content([], _FD) -> void;
process_content(Other, _FD) ->  io:format("conent is ??  ~p~n", [Other]).

%%%===================================================================
%%% Handles the parsing of all fields and enum values
%%%===================================================================
process_field(Element = #xmlElement{name=field, content=_Content, attributes=_Attributes}, FD) ->
    generate_field_parse(Element, FD);
process_field(_, _FD) -> void.

generate_field_parse(#xmlElement{name=field, content=Content, attributes=_Attributes} = Element, FD) ->
    Number = get_attribute(Element, number),
    Name = get_attribute(Element, name),
    Type = get_attribute(Element, type),

    io:format(FD, "field_parse(<<\"~s=\", Value/binary>>) -> ~n", [Number]),
    case length(Content) > 0 of
	true -> io:format(FD, "Val = case Value of~n",[]),  
		lists:map(fun(Elem) -> generate_enum_parse(Elem, FD) end, Content),
		io:format(FD, "_ -> unknown~n", []),
		io:format(FD, "end,~n",[]),
		io:format(FD, "{'~s', Val};~n", [Name]);
	false ->
	    case Type of
		"LENGTH" -> io:format(FD,"{'~s', to_int(Value)};~n", [Name]);
		"INT" -> io:format(FD,"{'~s', to_int(Value)};~n", [Name]);
		"SEQNUM" -> io:format(FD, "{'~s', to_int(Value)};~n", [Name]);
		"PRICE" -> io:format(FD, "{'~s', to_float(Value)};~n", [Name]);
		"CURRENCY" -> io:format(FD, "{'~s', to_float(Value)};~n", [Name]);
		"QTY" -> io:format(FD, "{'~s', to_float(Value)};~n", [Name]);
		_ -> io:format(FD, "{'~s', Value};~n", [Name])
	    end
    end.
   		      
generate_enum_parse(#xmlElement{} = Element, FD) ->
    Enum = get_attribute(Element, enum),
    Description = get_attribute(Element, description),
    io:format(FD, "<<\"~s\">> -> '~s'; ~n", [Enum, Description]);
generate_enum_parse(_, _FD) -> void.

%%%===================================================================
%%% Handles the parsing of messages, header and trailer
%%%===================================================================
generate_validator(#xmlElement{name=header} = Element, FD) ->
    RequiredList = get_required_fields(Element),
    io:format(FD, "required_header_fields() -> ~n~p.~n", [RequiredList]);

generate_validator(#xmlElement{name=trailer} = Element, FD) ->
    RequiredList = get_required_fields(Element),
    io:format(FD, "required_trailer_fields() -> ~n~p.~n~n", [RequiredList]);

generate_validator(#xmlElement{name=message} = Element, FD) ->
    RequiredList = get_required_fields(Element),
    Name = get_attribute(Element, name),
    io:format(FD, "required_fields('~s') -> ~n~p;~n", [Name, RequiredList]);

generate_validator(_,_) -> ok.

get_required_fields(#xmlElement{name=_, content=Content, attributes=_Attributes}) ->
    lists:foldl(
      fun(C = #xmlElement{}, Result) ->
	      Required = get_attribute(C, required),
	      Name = get_attribute(C, name),
	      case Required of
		  "Y" -> [list_to_atom(Name) | Result];
		  "N" -> Result
	      end;
	 (_, Result) -> Result
      end, [], Content).

%%%===================================================================
%%% Helpers
%%%===================================================================
get_attribute(#xmlElement{name=_, content=_Content, attributes=Attributes}, Name) ->
    [#xmlAttribute{value=Value, name=Name}] = [ X || X <- Attributes, X#xmlAttribute.name=:=Name ],
    Value.

get_field_def(Name, Doc) ->
    xmerl_xpath:string("/fix/fields/field[@name='"++Name++"']", Doc).

get_message_type(Value, Doc) ->
    xmerl_xpath:string("/fix/fields/field[@name='MsgType']/value[@enum='"++Value++"']", Doc).
