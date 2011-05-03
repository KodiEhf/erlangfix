-module(fix_parser_gen).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

main() ->
   {Doc, _} = xmerl_scan:file("FIX42.xml"),
   #xmlElement{name=_Name, content=Content} = Doc,
   io:format("conent.length is ~p~n", [length(Content)]),
   process_content(Content, Doc).

%%Process all /fix/fields/field tags
process_content([#xmlElement{name=fields, content=Content, attributes=Attributes} | Rest], Doc) ->  
    %io:format("tag is ~p~n", [fields]),
    lists:map(fun(C) -> generate_field_parse(C, Doc) end, Content);
%%All other tags are ignored
process_content([Other | Rest], Doc) -> process_content(Rest, Doc).

%process all xmlEmlements with name field
generate_field_parse(Elem = #xmlElement{name=field, content=Content, attributes=Attributes}, Doc) ->
    {Name, Number, Type} = get_field_attributes(Elem),
    %check if the field is a group or a simple element
    case get_group_def(Name, Doc) of
	[] -> generate_field(Content, Number, Name, Type); %the field is a simple field
	[Group | Groups] -> generate_group(Number, Group, Doc) %the field is a group
    end;
%other elements are ignored
generate_field_parse(Other, Doc) -> ok.

%Generate code for simple fields	    
generate_field(Content, Number, Name, Type) ->
    io:format("field_parse([<<\"~s=\", Value/binary>> | Rest]) -> ~n", [Number]),
    case length(Content) > 0 of
	true -> io:format("Val = case Value of~n"),  
		lists:map(fun(Elem) -> generate_enum_parse(Elem) end, Content),
		io:format("_ -> unknown~n"),
		io:format("end,~n"),
		io:format("[{'~s', Val} | field_parse(Rest)];~n", [Name]);
	false ->
	    case Type of
		"LENGTH" -> io:format("[{'~s', to_int(Value)} | field_parse(Rest)];~n", [Name]);
		"INT" -> io:format("[{'~s', to_int(Value)} | field_parse(Rest)];~n", [Name]);
		"SEQNUM" -> io:format("[{'~s', to_int(Value)} | field_parse(Rest)];~n", [Name]);
		"PRICE" -> io:format("[{'~s', to_float(Value)} | field_parse(Rest)];~n", [Name]);
		"CURRENCY" -> io:format("[{'~s', to_float(Value)} | field_parse(Rest)];~n", [Name]);
		"QTY" -> io:format("[{'~s', to_float(Value)} | field_parse(Rest)];~n", [Name]);
		_ -> io:format("[{'~s', Value} | field_parse(Rest)];~n", [Name])
	    end
    end.

%Generate code for repeating groups
generate_group(Number, #xmlElement{name=group, content=Content, attributes=Attributes}, Doc) ->
    io:format("field_parse([<<\"~s=\", Value/binary>> | Rest]) -> ~n", [Number]),
    io:format("NumOfGroupItems = list_to_integer(binary_to_list(Value)),~n"),
    
    AllFieldsInGroup = [ X || X <- Content, X#xmlElement.name=:=field],

    FirstFieldElement = hd(AllFieldsInGroup), 

    io:format("Group = fun~n"),
    generate_group_fun(FirstFieldElement, true, Doc),

    lists:map(fun (Elem) -> generate_group_fun(Elem, false, Doc) end, tl(AllFieldsInGroup)),
	       
    io:format("(_F, 0, Result, Tail) -> {Result, Tail}~n"),
    io:format("end,~n"),
    io:format("{Result, NewRest} = Group(Group, NumOfGroupItems, Rest),~n"),
    io:format("[Result | field_parse(NewRest)];~n").
    %io:format("Group is ~p~n", [Content]).

generate_group_fun(Elem, Count, Doc) ->
    FieldName = get_attribute(Elem, name),
    [FieldDef] = get_field_def(FieldName, Doc),
    {_Name, Number, _Type} = get_field_attributes(FieldDef),
    io:format("(F, Count, Result, [Field = <<\"~s=\", _V/binary>> | Tail]) ->~n", [Number]),
    case Count of
	true -> io:format("F(F, Count-1, [field_parse(Field) | Result], Tail);~n");
	false -> io:format("F(F, Count, [field_parse(Field) | Result], Tail);~n")
    end.

%Generate code for enum values
generate_enum_parse(#xmlElement{name=_Name, content=_Content, attributes=Attributes}) ->
    [A1] = [ X || X <- Attributes, X#xmlAttribute.name=:=enum ],
    [A2] = [ Y || Y <- Attributes, Y#xmlAttribute.name=:=description ],
    io:format("<<\"~s\">> -> '~s'; ~n", [A1#xmlAttribute.value, A2#xmlAttribute.value]);
generate_enum_parse(_) -> "".

% Helpers
get_field_attributes(#xmlElement{name=field, attributes=Attributes}) ->
    [#xmlAttribute{value=Number, name=number}] = [ X || X <- Attributes, X#xmlAttribute.name=:=number ],
    [#xmlAttribute{value=Name, name=name}] = [ Y || Y <- Attributes, Y#xmlAttribute.name=:=name ],
    [#xmlAttribute{value=Type, name=type}] = [ Z || Z <- Attributes, Z#xmlAttribute.name=:=type ],
    {Name, Number, Type}.

get_attribute(#xmlElement{name=field, content=Content, attributes=Attributes}, Name) ->
    [#xmlAttribute{value=Value, name=Name}] = [ X || X <- Attributes, X#xmlAttribute.name=:=Name ],
    Value.

get_group_def(Name, Doc) ->
    %%{Doc, _} = xmerl_scan:file("FIX42.xml"),
    %%io:format("/fix/messages/message/group[@name='"++Name++"']~n"),
    xmerl_xpath:string("/fix/messages/message/group[@name='"++Name++"']", Doc).

get_field_def(Name, Doc) ->
    xmerl_xpath:string("/fix/fields/field[@name='"++Name++"']", Doc).

%% process_content([#xmlElement{name=header, content=Content, attributes=Attributes} | Rest], Doc) ->  
%%     io:format("Group is ~p~n", [header]),
%%     io:format("parse_header([Elem | Rest], RequiredFieldsCount) -> ~n"),
%%     io:format("case Elem of ~n"),
%%     lists:map(fun(C) -> process_field(C, Doc) end, Content),
%%     io:format("end~n");
%%    %% process_content(Rest, Level);

%% process_content([Other | Rest], Doc) ->
%%    process_content(Rest, Doc);

%% process_content(Other, Doc) -> void.

%% get_field_def(Name, Doc) ->
%%     %%{Doc, _} = xmerl_scan:file("FIX42.xml"),
%%     %%io:format("/fix/fields/field[@name='"++Name++"']~n"),
%%     xmerl_xpath:string("/fix/fields/field[@name='"++Name++"']", Doc).

%% process_field(#xmlElement{name=field, content=Content, attributes=Attributes}, Doc) ->
%%     [#xmlAttribute{value=Name, name=name}] = [ _X || _X <- Attributes, _X#xmlAttribute.name=:=name ],
%%     [#xmlElement{name=field, attributes=FieldDefAttributes}] = get_field_def(Name, Doc),
%%     [#xmlAttribute{value=Number, name=number}] = [ _Y || _Y <- FieldDefAttributes, _Y#xmlAttribute.name=:=number ],
%%     io:format("<<~p=, Value/binary>> -> [parse_field(Elem) | parse_header(Rest, RequiredFieldscount)];~n", [Number]);
    
%%     %io:format("Def = ~p~n", [Def]);

%% process_field(Other, Doc) -> ok.


%% test() ->
%%     xmerl_sax_parser:file("FIX42.xml",
%%                        [{event_fun,
%%                          fun(Event, _Location, _State) ->
%%                                  io:format("~p~n", [Event])
%%                          end}]).


%% process(_Event = {startElement, _, "header", _, _},
%%      _Location,
%%      _State) ->
%%     io:format("parse_header() -> ~n");
