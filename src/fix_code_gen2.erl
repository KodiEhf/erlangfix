-module(fix_code_gen2).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

main() ->
    {#xmlElement{name=_Name, content=Content}, _} = xmerl_scan:file("Fix42_inet.xml"),
    {ok, FD} = file:open("result_parser.erl", [write]),
    io:format(FD, "-module(result_parser).~n",[]),
    io:format(FD, "-compile(export_all).~n",[]),
    process_doc(#xmlElement{name=_Name, content=Content}, FD),
    file:close(FD).

process_doc(Doc, FD) ->
    %Split the document into its 4 parts
    Fields = xmerl_xpath:string("/fix/fields/field", Doc),
    Messages = xmerl_xpath:string("/fix/messages/message", Doc),
    [Header] = xmerl_xpath:string("/fix/header", Doc),
    [Trailer] = xmerl_xpath:string("/fix/trailer", Doc),
    %generate the records for the header, messages and the trailer.
    generate_record(Header, FD),
    lists:map(fun(M) -> generate_record(M, FD) end, Messages),
    generate_record(Trailer, FD),
    generate_validate(Header, FD),
    lists:map(fun(M) -> generate_validate(M, FD) end, Messages),
    generate_validate(Trailer, FD),
    %create the parsers for the header, messages and the trailer
    generate_parser(Messages, Doc, FD),
    generate_parser(Header, Doc, FD),
    generate_parser(Trailer, Doc, FD),
    generate_message_parser(Messages, Doc, FD),
    generate_field_parser(Fields, Doc, FD).

generate_validate(#xmlElement{name=field}=Field, Record, FD) ->
    Name = get_attribute(Field, name),
    io:format(FD, "Message#'~s'.'~s' =:= undefined -> {error, \"Required field '~s' missing\"};~n", [Record, Name, Name]).

generate_validate(#xmlElement{name=header, content=Content}, FD) ->
    io:format(FD, "validate(Message = #'Header'{}) ->~n", []),
    io:format(FD, "if~n", []),
    Fields = lists:filter(fun(X) -> is_record(X, xmlElement) andalso get_attribute(X, required) =:= "Y" end, Content),
    lists:map(fun(F) -> generate_validate(F, 'Header', FD) end, Fields),   
    io:format(FD, "true -> ok~n", []),
    io:format(FD, "end;~n", []);

generate_validate(#xmlElement{name=message, content=Content} = Message, FD) ->
    Fields = lists:filter(fun(X) -> is_record(X, xmlElement) andalso get_attribute(X, required) =:= "Y" end, Content),
    case Fields of
	[] -> ok;
	_ ->
	    Name = get_attribute(Message, name),
	    io:format(FD, "validate(Message = #'~s'{}) ->~n", [Name]),
	    io:format(FD, "if~n", []),
	    lists:map(fun(F) -> generate_validate(F, Name, FD) end, Fields),   
	    io:format(FD, "true -> ok~n", []),
	    io:format(FD, "end;~n", [])
    end;

generate_validate(#xmlElement{name=trailer, content=Content}, FD) ->
    io:format(FD, "validate(Message = #'Trailer'{}) ->~n", []),
    io:format(FD, "if~n", []),
    Fields = lists:filter(fun(X) -> is_record(X, xmlElement) andalso get_attribute(X, required) =:= "Y" end, Content),
    lists:map(fun(F) -> generate_validate(F, 'Trailer', FD) end, Fields),   
    io:format(FD, "true -> ok~n", []),
    io:format(FD, "end.~n", []).


generate_field_parser(Fields, Doc, FD) ->
    lists:map(fun(F) -> generate_parser(F, Doc, FD) end, Fields),  
    io:format(FD, "parse_field(<<>>) -> unknown.~n", []).

generate_message_parser(Messages, Doc, FD) ->
     lists:map(fun(M) -> 
		       generate_parser(M, Doc, FD),
		       case lists:last(Messages) of
			   M -> io:format(FD, ".~n~n~n", []);
			   _ -> io:format(FD, ";~n", [])
		       end
	       end, Messages).

generate_record(#xmlElement{name=message} = Element, FD) ->
    Name = get_attribute(Element, name),
    io:format(FD, "-record('~s', {", [Name]),
    print_fields(FD, get_fields(Element)),
    io:format(FD, "}).~n", []);

generate_record(#xmlElement{name=header} = Element, FD) ->
    io:format(FD, "-record('~s', {", ['Header']),
    print_fields(FD, get_fields(Element)),
    io:format(FD, "}).~n", []);

generate_record(#xmlElement{name=trailer} = Element, FD) ->
    io:format(FD, "-record('~s', {", ['Trailer']),
    print_fields(FD, get_fields(Element)),
    io:format(FD, "}).~n", []).

print_fields(FD, [Field]) ->
    io:format(FD, "~p", [Field]);
print_fields(FD, [Field|Fields]) ->
    io:format(FD, "~p,", [Field]),
    print_fields(FD, Fields).

get_fields(#xmlElement{name=_, content=Content, attributes=_Attributes}) ->
    lists:foldl(
      fun(C = #xmlElement{}, Result) ->
	      Name = get_attribute(C, name),
	      [list_to_atom(Name) | Result];
	 (_, Result) -> Result
      end, [], Content).

get_field_defs(#xmlElement{name=_, content=Content, attributes=_Attributes}, Doc) ->
    lists:foldl(
      fun(C = #xmlElement{name=field}, Result) ->
	      FieldName = get_attribute(C, name),
	      [Field] = xmerl_xpath:string("/fix/fields/field[@name='"++FieldName++"']", Doc),
	      io:format("Field is ~p~n", [Field]),
	      Name = get_attribute(Field, name),
	      Tag =  get_attribute(Field, number),
	      [{Tag, list_to_atom(Name)} | Result];
	 (_, Result) -> Result
      end, [], Content).

generate_parser(Messages, _Doc, FD) when is_list(Messages) ->
    io:format(FD, "parse_field_list(FieldList) ->~n", []),
    io:format(FD, "{Header, Rest} = parse_header(#'Header'{}, FieldList),~n", []),
    io:format(FD, "{Message, NewRest} = ~n", []),    
    io:format(FD, "case Header#'Header'.'MsgType' of~n", []),
    lists:map(fun(#xmlElement{name=message} = Element) -> 
		      Name = get_attribute(Element, name),
		      io:format(FD, "'~s'-> ", [to_upper(Name)]),
		      case lists:last(Messages) of
			  Element -> io:format(FD, "parse_message(#'~s'{}, Rest)~n", [Name]);
			  _ -> io:format(FD, "parse_message(#'~s'{}, Rest);~n", [Name])
		      end
	      end, Messages),    
    io:format(FD, "end,~n", []),
    io:format(FD, "Trailer = parse_trailer(#'Trailer'{}, NewRest),~n", []),
    io:format(FD, "{Header, Message, Trailer}.~n", []);

generate_parser(#xmlElement{name=header} = Element, Doc, FD) ->
    io:format(FD, "parse_header(Header = #'Header'{}, [Field|Fields]) ->~n", []),
    io:format(FD, "case parse_field(Field) of~n", []),
    FieldDefs = get_field_defs(Element, Doc),
    io:format("~p~n", [FieldDefs]),
    lists:map(fun({_Tag, Field}) -> 
		      io:format(FD, "{'~s', Value} -> ", [Field]),
		      io:format(FD, "parse_header(Header#'Header'{'~s' = Value}, Fields);~n", [Field])
	      end, FieldDefs),    
    io:format(FD, "_ -> {Header, [Field|Fields]}~n", []),
    io:format(FD, "end.~n", []);

generate_parser(#xmlElement{name=trailer} = Element, Doc, FD) ->
    io:format(FD, "parse_trailer(Trailer = #'Trailer'{}, [Field|Fields]) ->~n", []),
    io:format(FD, "case parse_field(Field) of~n", []),
    FieldDefs = get_field_defs(Element, Doc),
    io:format("~p~n", [FieldDefs]),
    lists:map(fun({_Tag, Field}) -> 
		      io:format(FD, "{'~s', Value} -> ", [Field]),
		      io:format(FD, "parse_trailer(Trailer#'Trailer'{'~s' = Value}, Fields);~n", [Field])
	      end, FieldDefs),    
    io:format(FD, "_ -> Trailer~n", []),
    io:format(FD, "end.~n", []);

generate_parser(#xmlElement{name=message} = Element, Doc, FD) ->
    Name = get_attribute(Element, name),
    io:format(FD, "parse_message(Message = #'"++Name++"'{}, [Field|Fields]) ->~n", []),
    io:format(FD, "case parse_field(Field) of~n", []),
    FieldDefs = get_field_defs(Element, Doc),
    io:format("~p~n", [FieldDefs]),
    lists:map(fun({_Tag, Field}) -> 
		      io:format(FD, "{'~s', Value} -> ", [Field]),
		      io:format(FD, "parse_message(Message#'"++Name++"'{'~s' = Value}, Fields);~n", [Field])
	      end, FieldDefs),    
    io:format(FD, "_ -> {Message, [Field|Fields]}~n",[]),
    io:format(FD, "end", []);

generate_parser(#xmlElement{name=field, content=[]} = Element, _Doc, FD) -> 
    Name = get_attribute(Element, name),
    Number = get_attribute(Element, number),
    Type = get_attribute(Element, type),
    case Type of
	"INT" -> io:format(FD, "parse_field(<<\"~s=\", Value/binary>>) -> {'~s', binary_to_int(Value)};~n", [Number, Name]);
	_ -> io:format(FD, "parse_field(<<\"~s=\", Value/binary>>) -> {'~s', Value};~n", [Number, Name])
    end;

generate_parser(#xmlElement{name=field, content=Content} = Element, _Doc, FD) ->
    Name = get_attribute(Element, name),
    Number = get_attribute(Element, number),
    io:format(FD, "parse_field(<<\"~s=\", Value/binary>>) ->~n", [Number]),
    io:format(FD, "case Value of~n", []),
    Values =  lists:filter(fun(X) -> is_record(X, xmlElement) end, Content),
    lists:map(fun(#xmlElement{name=value} = Value) -> 
		      Enum = get_attribute(Value, enum),
		      Description = get_attribute(Value, description),
		      io:format(FD, "<<~p>> -> {'~s','~s'};~n", [Enum, Name, Description])
	      end, Values),
    io:format(FD, "_ -> {'~s',Value}~n", [Name]),
    io:format(FD, "end;~n", []).

to_upper(Name) -> tl(format_message_type(Name)).
format_message_type([]) -> [];
format_message_type([Char|Rest]) -> 
    io:format("Char is ~p~n", [Char]),
    case string:to_upper(Char) of
	Char -> "_" ++[Char]++format_message_type(Rest);
	Upper -> [Upper]++format_message_type(Rest)
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================
%% Gets the attributes for a given field
get_attribute(#xmlElement{name=_, content=_Content, attributes=Attributes}, Name) ->
    io:format("get_attribute ~p~n", [Name]),
    try
	[#xmlAttribute{value=Value, name=Name}] = [ X || X <- Attributes, X#xmlAttribute.name=:=Name ],
	Value
    catch _Error ->
	    io:format("blimm~n")
    end.
