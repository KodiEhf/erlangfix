-module(fix_parser).
-include("fix.hrl").
-export([parse/2]).

%% We need a mapping of integers to tag-names
%% We know that a message ends with "10=xyz |"
%% Parse should return a list of "parsed messages" and a binary rest
%% BodyLength is always the second field in the message,
%% length refers to the message length up to checksum field

parse(CurrentMessage, MessageLength) ->
    MessageSplit = binary:split(CurrentMessage, [<<?SOH>>], [global]),
    {Header, FixMessage, Trailer} = parse_field_list(MessageSplit),
    <<CheckSumData:MessageLength/binary, _/binary>> = <<CurrentMessage/binary>>,
    check_sum(CheckSumData), %% todo compare checksums
    validate(Header),
    validate(FixMessage),
    validate(Trailer),
    {Header, FixMessage, Trailer}.


check_sum(<<Message/binary>>) ->
    List = binary_to_list(Message),
    lists:sum(List) rem 256.

to_int(Binary) ->
    list_to_integer(binary_to_list(Binary)).

to_float(Binary) -> Binary.

validate(Message = #'Header'{}) ->
    if
	Message#'Header'.'BeginString' =:= undefined -> {error, "Required field 'BeginString' missing"};
	Message#'Header'.'BodyLength' =:= undefined -> {error, "Required field 'BodyLength' missing"};
	Message#'Header'.'MsgType' =:= undefined -> {error, "Required field 'MsgType' missing"};
	Message#'Header'.'MsgSeqNum' =:= undefined -> {error, "Required field 'MsgSeqNum' missing"};
	Message#'Header'.'SenderCompID' =:= undefined -> {error, "Required field 'SenderCompID' missing"};
	Message#'Header'.'SenderSubID' =:= undefined -> {error, "Required field 'SenderSubID' missing"};
	Message#'Header'.'SendingTime' =:= undefined -> {error, "Required field 'SendingTime' missing"};
	Message#'Header'.'TargetCompID' =:= undefined -> {error, "Required field 'TargetCompID' missing"};
	Message#'Header'.'TargetSubID' =:= undefined -> {error, "Required field 'TargetSubID' missing"};
	true -> ok
    end;
validate(Message = #'Logon'{}) ->
    if
	Message#'Logon'.'EncryptMethod' =:= undefined -> {error, "Required field 'EncryptMethod' missing"};
	Message#'Logon'.'HeartBtInt' =:= undefined -> {error, "Required field 'HeartBtInt' missing"};
	true -> ok
    end;
validate(Message = #'TestRequest'{}) ->
    if
	Message#'TestRequest'.'TestReqID' =:= undefined -> {error, "Required field 'TestReqID' missing"};
	true -> ok
    end;
validate(Message = #'ResendRequest'{}) ->
    if
	Message#'ResendRequest'.'BeginSeqNo' =:= undefined -> {error, "Required field 'BeginSeqNo' missing"};
	Message#'ResendRequest'.'EndSeqNo' =:= undefined -> {error, "Required field 'EndSeqNo' missing"};
	true -> ok
    end;
validate(Message = #'Reject'{}) ->
    if
	Message#'Reject'.'RefSeqNum' =:= undefined -> {error, "Required field 'RefSeqNum' missing"};
	true -> ok
    end;
validate(Message = #'SequenceReset'{}) ->
    if
	Message#'SequenceReset'.'NewSeqNo' =:= undefined -> {error, "Required field 'NewSeqNo' missing"};
	true -> ok
    end;
validate(Message = #'NewOrderSingle'{}) ->
    if
	Message#'NewOrderSingle'.'ClOrdID' =:= undefined -> {error, "Required field 'ClOrdID' missing"};
	Message#'NewOrderSingle'.'HandlInst' =:= undefined -> {error, "Required field 'HandlInst' missing"};
	Message#'NewOrderSingle'.'OrdType' =:= undefined -> {error, "Required field 'OrdType' missing"};
	Message#'NewOrderSingle'.'Side' =:= undefined -> {error, "Required field 'Side' missing"};
	Message#'NewOrderSingle'.'Symbol' =:= undefined -> {error, "Required field 'Symbol' missing"};
	Message#'NewOrderSingle'.'TransactTime' =:= undefined -> {error, "Required field 'TransactTime' missing"};
	true -> ok
    end;
validate(Message = #'ExecutionReport'{}) ->
    if
	Message#'ExecutionReport'.'AvgPx' =:= undefined -> {error, "Required field 'AvgPx' missing"};
	Message#'ExecutionReport'.'CumQty' =:= undefined -> {error, "Required field 'CumQty' missing"};
	Message#'ExecutionReport'.'ExecID' =:= undefined -> {error, "Required field 'ExecID' missing"};
	Message#'ExecutionReport'.'ExecTransType' =:= undefined -> {error, "Required field 'ExecTransType' missing"};
	Message#'ExecutionReport'.'OrderID' =:= undefined -> {error, "Required field 'OrderID' missing"};
	Message#'ExecutionReport'.'OrdStatus' =:= undefined -> {error, "Required field 'OrdStatus' missing"};
	Message#'ExecutionReport'.'Side' =:= undefined -> {error, "Required field 'Side' missing"};
	Message#'ExecutionReport'.'ExecType' =:= undefined -> {error, "Required field 'ExecType' missing"};
	Message#'ExecutionReport'.'LeavesQty' =:= undefined -> {error, "Required field 'LeavesQty' missing"};
	true -> ok
    end;
validate(Message = #'OrderCancelReplaceRequest'{}) ->
    if
	Message#'OrderCancelReplaceRequest'.'ClOrdID' =:= undefined -> {error, "Required field 'ClOrdID' missing"};
	Message#'OrderCancelReplaceRequest'.'HandlInst' =:= undefined -> {error, "Required field 'HandlInst' missing"};
	Message#'OrderCancelReplaceRequest'.'OrderID' =:= undefined -> {error, "Required field 'OrderID' missing"};
	Message#'OrderCancelReplaceRequest'.'OrderQty' =:= undefined -> {error, "Required field 'OrderQty' missing"};
	Message#'OrderCancelReplaceRequest'.'OrdType' =:= undefined -> {error, "Required field 'OrdType' missing"};
	Message#'OrderCancelReplaceRequest'.'OrigClOrdID' =:= undefined -> {error, "Required field 'OrigClOrdID' missing"};
	Message#'OrderCancelReplaceRequest'.'Side' =:= undefined -> {error, "Required field 'Side' missing"};
	Message#'OrderCancelReplaceRequest'.'Symbol' =:= undefined -> {error, "Required field 'Symbol' missing"};
	Message#'OrderCancelReplaceRequest'.'TransactTime' =:= undefined -> {error, "Required field 'TransactTime' missing"};
	true -> ok
    end;
validate(Message = #'OrderCancelRequest'{}) ->
    if
	Message#'OrderCancelRequest'.'ClOrdID' =:= undefined -> {error, "Required field 'ClOrdID' missing"};
	Message#'OrderCancelRequest'.'OrderID' =:= undefined -> {error, "Required field 'OrderID' missing"};
	Message#'OrderCancelRequest'.'OrderQty' =:= undefined -> {error, "Required field 'OrderQty' missing"};
	Message#'OrderCancelRequest'.'OrigClOrdID' =:= undefined -> {error, "Required field 'OrigClOrdID' missing"};
	Message#'OrderCancelRequest'.'Side' =:= undefined -> {error, "Required field 'Side' missing"};
	Message#'OrderCancelRequest'.'Symbol' =:= undefined -> {error, "Required field 'Symbol' missing"};
	Message#'OrderCancelRequest'.'TransactTime' =:= undefined -> {error, "Required field 'TransactTime' missing"};
	true -> ok
    end;
validate(Message = #'OrderCancelReject'{}) ->
    if
	Message#'OrderCancelReject'.'ClOrdID' =:= undefined -> {error, "Required field 'ClOrdID' missing"};
	Message#'OrderCancelReject'.'OrderID' =:= undefined -> {error, "Required field 'OrderID' missing"};
	Message#'OrderCancelReject'.'OrdStatus' =:= undefined -> {error, "Required field 'OrdStatus' missing"};
	Message#'OrderCancelReject'.'OrigClOrdID' =:= undefined -> {error, "Required field 'OrigClOrdID' missing"};
	Message#'OrderCancelReject'.'ClientID' =:= undefined -> {error, "Required field 'ClientID' missing"};
	Message#'OrderCancelReject'.'CxlRejResponseTo' =:= undefined -> {error, "Required field 'CxlRejResponseTo' missing"};
	true -> ok
    end;
validate(Message = #'Trailer'{}) ->
    if
	Message#'Trailer'.'CheckSum' =:= undefined -> {error, "Required field 'CheckSum' missing"};
	true -> ok
    end.

parse_field_list(FieldList) ->
    {Header, Rest} = parse_header(#'Header'{}, FieldList),
    {Message, NewRest} = 
	case Header#'Header'.'MsgType' of
	    'HEARTBEAT'-> parse_message(#'Heartbeat'{}, Rest);
	    'LOGON'-> parse_message(#'Logon'{}, Rest);
	    'TEST_REQUEST'-> parse_message(#'TestRequest'{}, Rest);
	    'RESEND_REQUEST'-> parse_message(#'ResendRequest'{}, Rest);
	    'REJECT'-> parse_message(#'Reject'{}, Rest);
	    'SEQUENCE_RESET'-> parse_message(#'SequenceReset'{}, Rest);
	    'LOGOUT'-> parse_message(#'Logout'{}, Rest);
	    'NEW_ORDER_SINGLE'-> parse_message(#'NewOrderSingle'{}, Rest);
	    'EXECUTION_REPORT'-> parse_message(#'ExecutionReport'{}, Rest);
	    'ORDER_CANCEL_REPLACE_REQUEST'-> parse_message(#'OrderCancelReplaceRequest'{}, Rest);
	    'ORDER_CANCEL_REQUEST'-> parse_message(#'OrderCancelRequest'{}, Rest);
	    'ORDER_CANCEL_REJECT'-> parse_message(#'OrderCancelReject'{}, Rest)
	end,
    Trailer = parse_trailer(#'Trailer'{}, NewRest),
    {Header, Message, Trailer}.
parse_header(Header = #'Header'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'DeliverToSubID', Value} -> parse_header(Header#'Header'{'DeliverToSubID' = Value}, Fields);
	{'DeliverToCompID', Value} -> parse_header(Header#'Header'{'DeliverToCompID' = Value}, Fields);
	{'OrigSendingTime', Value} -> parse_header(Header#'Header'{'OrigSendingTime' = Value}, Fields);
	{'OnBehalfOfSubID', Value} -> parse_header(Header#'Header'{'OnBehalfOfSubID' = Value}, Fields);
	{'OnBehalfOfCompID', Value} -> parse_header(Header#'Header'{'OnBehalfOfCompID' = Value}, Fields);
	{'PossResend', Value} -> parse_header(Header#'Header'{'PossResend' = Value}, Fields);
	{'TargetSubID', Value} -> parse_header(Header#'Header'{'TargetSubID' = Value}, Fields);
	{'TargetCompID', Value} -> parse_header(Header#'Header'{'TargetCompID' = Value}, Fields);
	{'SendingTime', Value} -> parse_header(Header#'Header'{'SendingTime' = Value}, Fields);
	{'SenderSubID', Value} -> parse_header(Header#'Header'{'SenderSubID' = Value}, Fields);
	{'SenderCompID', Value} -> parse_header(Header#'Header'{'SenderCompID' = Value}, Fields);
	{'PossDupFlag', Value} -> parse_header(Header#'Header'{'PossDupFlag' = Value}, Fields);
	{'MsgSeqNum', Value} -> parse_header(Header#'Header'{'MsgSeqNum' = Value}, Fields);
	{'MsgType', Value} -> parse_header(Header#'Header'{'MsgType' = Value}, Fields);
	{'BodyLength', Value} -> parse_header(Header#'Header'{'BodyLength' = Value}, Fields);
	{'BeginString', Value} -> parse_header(Header#'Header'{'BeginString' = Value}, Fields);
	_ -> {Header, [Field|Fields]}
    end.
parse_trailer(Trailer = #'Trailer'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'CheckSum', Value} -> parse_trailer(Trailer#'Trailer'{'CheckSum' = Value}, Fields);
	_ -> Trailer
    end.
parse_message(Message = #'Heartbeat'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'TestReqID', Value} -> parse_message(Message#'Heartbeat'{'TestReqID' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'Logon'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'ResetSeqNumFlag', Value} -> parse_message(Message#'Logon'{'ResetSeqNumFlag' = Value}, Fields);
	{'HeartBtInt', Value} -> parse_message(Message#'Logon'{'HeartBtInt' = Value}, Fields);
	{'EncryptMethod', Value} -> parse_message(Message#'Logon'{'EncryptMethod' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'TestRequest'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'TestReqID', Value} -> parse_message(Message#'TestRequest'{'TestReqID' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'ResendRequest'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'EndSeqNo', Value} -> parse_message(Message#'ResendRequest'{'EndSeqNo' = Value}, Fields);
	{'BeginSeqNo', Value} -> parse_message(Message#'ResendRequest'{'BeginSeqNo' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'Reject'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'SessionRejectReason', Value} -> parse_message(Message#'Reject'{'SessionRejectReason' = Value}, Fields);
	{'RefMsgType', Value} -> parse_message(Message#'Reject'{'RefMsgType' = Value}, Fields);
	{'RefTagID', Value} -> parse_message(Message#'Reject'{'RefTagID' = Value}, Fields);
	{'Text', Value} -> parse_message(Message#'Reject'{'Text' = Value}, Fields);
	{'RefSeqNum', Value} -> parse_message(Message#'Reject'{'RefSeqNum' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'SequenceReset'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'GapFillFlag', Value} -> parse_message(Message#'SequenceReset'{'GapFillFlag' = Value}, Fields);
	{'NewSeqNo', Value} -> parse_message(Message#'SequenceReset'{'NewSeqNo' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'Logout'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'Text', Value} -> parse_message(Message#'Logout'{'Text' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'NewOrderSingle'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'BrSeqNbr', Value} -> parse_message(Message#'NewOrderSingle'{'BrSeqNbr' = Value}, Fields);
	{'CrossTradeFlag', Value} -> parse_message(Message#'NewOrderSingle'{'CrossTradeFlag' = Value}, Fields);
	{'DisplayInst', Value} -> parse_message(Message#'NewOrderSingle'{'DisplayInst' = Value}, Fields);
	{'ClRefID', Value} -> parse_message(Message#'NewOrderSingle'{'ClRefID' = Value}, Fields);
	{'SubMktID', Value} -> parse_message(Message#'NewOrderSingle'{'SubMktID' = Value}, Fields);
	{'OrderRestrictions', Value} -> parse_message(Message#'NewOrderSingle'{'OrderRestrictions' = Value}, Fields);
	{'OrderCapacity', Value} -> parse_message(Message#'NewOrderSingle'{'OrderCapacity' = Value}, Fields);
	{'ClearingAccount', Value} -> parse_message(Message#'NewOrderSingle'{'ClearingAccount' = Value}, Fields);
	{'ClearingFirm', Value} -> parse_message(Message#'NewOrderSingle'{'ClearingFirm' = Value}, Fields);
	{'PegDifference', Value} -> parse_message(Message#'NewOrderSingle'{'PegDifference' = Value}, Fields);
	{'ExpireTime', Value} -> parse_message(Message#'NewOrderSingle'{'ExpireTime' = Value}, Fields);
	{'MaxFloor', Value} -> parse_message(Message#'NewOrderSingle'{'MaxFloor' = Value}, Fields);
	{'MinQty', Value} -> parse_message(Message#'NewOrderSingle'{'MinQty' = Value}, Fields);
	{'ExecBroker', Value} -> parse_message(Message#'NewOrderSingle'{'ExecBroker' = Value}, Fields);
	{'TransactTime', Value} -> parse_message(Message#'NewOrderSingle'{'TransactTime' = Value}, Fields);
	{'TimeInForce', Value} -> parse_message(Message#'NewOrderSingle'{'TimeInForce' = Value}, Fields);
	{'Symbol', Value} -> parse_message(Message#'NewOrderSingle'{'Symbol' = Value}, Fields);
	{'Side', Value} -> parse_message(Message#'NewOrderSingle'{'Side' = Value}, Fields);
	{'SecurityID', Value} -> parse_message(Message#'NewOrderSingle'{'SecurityID' = Value}, Fields);
	{'Price', Value} -> parse_message(Message#'NewOrderSingle'{'Price' = Value}, Fields);
	{'OrdType', Value} -> parse_message(Message#'NewOrderSingle'{'OrdType' = Value}, Fields);
	{'OrderQty', Value} -> parse_message(Message#'NewOrderSingle'{'OrderQty' = Value}, Fields);
	{'HandlInst', Value} -> parse_message(Message#'NewOrderSingle'{'HandlInst' = Value}, Fields);
	{'ExecInst', Value} -> parse_message(Message#'NewOrderSingle'{'ExecInst' = Value}, Fields);
	{'Currency', Value} -> parse_message(Message#'NewOrderSingle'{'Currency' = Value}, Fields);
	{'ClOrdID', Value} -> parse_message(Message#'NewOrderSingle'{'ClOrdID' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'ExecutionReport'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'LockedIn', Value} -> parse_message(Message#'ExecutionReport'{'LockedIn' = Value}, Fields);
	{'TradingsessionSubId', Value} -> parse_message(Message#'ExecutionReport'{'TradingsessionSubId' = Value}, Fields);
	{'TradeLinkId', Value} -> parse_message(Message#'ExecutionReport'{'TradeLinkId' = Value}, Fields);
	{'SecondaryTradeReportRefID', Value} -> parse_message(Message#'ExecutionReport'{'SecondaryTradeReportRefID' = Value}, Fields);
	{'TradeReportRefId', Value} -> parse_message(Message#'ExecutionReport'{'TradeReportRefId' = Value}, Fields);
	{'TimestampCounterpart', Value} -> parse_message(Message#'ExecutionReport'{'TimestampCounterpart' = Value}, Fields);
	{'TimestampOwn', Value} -> parse_message(Message#'ExecutionReport'{'TimestampOwn' = Value}, Fields);
	{'DisseminationTime', Value} -> parse_message(Message#'ExecutionReport'{'DisseminationTime' = Value}, Fields);
	{'LockedInStatus', Value} -> parse_message(Message#'ExecutionReport'{'LockedInStatus' = Value}, Fields);
	{'BreakIndicator', Value} -> parse_message(Message#'ExecutionReport'{'BreakIndicator' = Value}, Fields);
	{'PriceType', Value} -> parse_message(Message#'ExecutionReport'{'PriceType' = Value}, Fields);
	{'StrikeTime', Value} -> parse_message(Message#'ExecutionReport'{'StrikeTime' = Value}, Fields);
	{'TradeReportRejectReason', Value} -> parse_message(Message#'ExecutionReport'{'TradeReportRejectReason' = Value}, Fields);
	{'ContraClearingAcct', Value} -> parse_message(Message#'ExecutionReport'{'ContraClearingAcct' = Value}, Fields);
	{'ContraTradePA', Value} -> parse_message(Message#'ExecutionReport'{'ContraTradePA' = Value}, Fields);
	{'DelayedDissemination', Value} -> parse_message(Message#'ExecutionReport'{'DelayedDissemination' = Value}, Fields);
	{'OverrideFlag', Value} -> parse_message(Message#'ExecutionReport'{'OverrideFlag' = Value}, Fields);
	{'ClearingPrice', Value} -> parse_message(Message#'ExecutionReport'{'ClearingPrice' = Value}, Fields);
	{'MICCode', Value} -> parse_message(Message#'ExecutionReport'{'MICCode' = Value}, Fields);
	{'RFQReferenceNo', Value} -> parse_message(Message#'ExecutionReport'{'RFQReferenceNo' = Value}, Fields);
	{'ContraOrderRestrictions', Value} -> parse_message(Message#'ExecutionReport'{'ContraOrderRestrictions' = Value}, Fields);
	{'Memo', Value} -> parse_message(Message#'ExecutionReport'{'Memo' = Value}, Fields);
	{'TrdRptStatus', Value} -> parse_message(Message#'ExecutionReport'{'TrdRptStatus' = Value}, Fields);
	{'TradeReportType', Value} -> parse_message(Message#'ExecutionReport'{'TradeReportType' = Value}, Fields);
	{'PublishTrdIndicator', Value} -> parse_message(Message#'ExecutionReport'{'PublishTrdIndicator' = Value}, Fields);
	{'TrdType', Value} -> parse_message(Message#'ExecutionReport'{'TrdType' = Value}, Fields);
	{'ReversalIndicator', Value} -> parse_message(Message#'ExecutionReport'{'ReversalIndicator' = Value}, Fields);
	{'ClearingInstruction', Value} -> parse_message(Message#'ExecutionReport'{'ClearingInstruction' = Value}, Fields);
	{'TradeReportID', Value} -> parse_message(Message#'ExecutionReport'{'TradeReportID' = Value}, Fields);
	{'LiquidityFlag', Value} -> parse_message(Message#'ExecutionReport'{'LiquidityFlag' = Value}, Fields);
	{'BrSeqNbr', Value} -> parse_message(Message#'ExecutionReport'{'BrSeqNbr' = Value}, Fields);
	{'CrossTradeFlag', Value} -> parse_message(Message#'ExecutionReport'{'CrossTradeFlag' = Value}, Fields);
	{'DisplayInst', Value} -> parse_message(Message#'ExecutionReport'{'DisplayInst' = Value}, Fields);
	{'ClRefID', Value} -> parse_message(Message#'ExecutionReport'{'ClRefID' = Value}, Fields);
	{'SubMktID', Value} -> parse_message(Message#'ExecutionReport'{'SubMktID' = Value}, Fields);
	{'TradeId', Value} -> parse_message(Message#'ExecutionReport'{'TradeId' = Value}, Fields);
	{'TrdMatchId', Value} -> parse_message(Message#'ExecutionReport'{'TrdMatchId' = Value}, Fields);
	{'OrderRestrictions', Value} -> parse_message(Message#'ExecutionReport'{'OrderRestrictions' = Value}, Fields);
	{'OrderCapacity', Value} -> parse_message(Message#'ExecutionReport'{'OrderCapacity' = Value}, Fields);
	{'TransBkdTime', Value} -> parse_message(Message#'ExecutionReport'{'TransBkdTime' = Value}, Fields);
	{'ClearingAccount', Value} -> parse_message(Message#'ExecutionReport'{'ClearingAccount' = Value}, Fields);
	{'ClearingFirm', Value} -> parse_message(Message#'ExecutionReport'{'ClearingFirm' = Value}, Fields);
	{'NoContraBrokers', Value} -> parse_message(Message#'ExecutionReport'{'NoContraBrokers' = Value}, Fields);
	{'ContraBroker', Value} -> parse_message(Message#'ExecutionReport'{'ContraBroker' = Value}, Fields);
	{'SettlBrkrCode', Value} -> parse_message(Message#'ExecutionReport'{'SettlBrkrCode' = Value}, Fields);
	{'LeavesQty', Value} -> parse_message(Message#'ExecutionReport'{'LeavesQty' = Value}, Fields);
	{'ExecType', Value} -> parse_message(Message#'ExecutionReport'{'ExecType' = Value}, Fields);
	{'ExpireTime', Value} -> parse_message(Message#'ExecutionReport'{'ExpireTime' = Value}, Fields);
	{'PegDifference', Value} -> parse_message(Message#'ExecutionReport'{'PegDifference' = Value}, Fields);
	{'MaxFloor', Value} -> parse_message(Message#'ExecutionReport'{'MaxFloor' = Value}, Fields);
	{'MinQty', Value} -> parse_message(Message#'ExecutionReport'{'MinQty' = Value}, Fields);
	{'ClientID', Value} -> parse_message(Message#'ExecutionReport'{'ClientID' = Value}, Fields);
	{'ExecBroker', Value} -> parse_message(Message#'ExecutionReport'{'ExecBroker' = Value}, Fields);
	{'SecurityDesc', Value} -> parse_message(Message#'ExecutionReport'{'SecurityDesc' = Value}, Fields);
	{'ProcessCode', Value} -> parse_message(Message#'ExecutionReport'{'ProcessCode' = Value}, Fields);
	{'FutSettDate', Value} -> parse_message(Message#'ExecutionReport'{'FutSettDate' = Value}, Fields);
	{'TradeDate', Value} -> parse_message(Message#'ExecutionReport'{'TradeDate' = Value}, Fields);
	{'TransactTime', Value} -> parse_message(Message#'ExecutionReport'{'TransactTime' = Value}, Fields);
	{'TimeInForce', Value} -> parse_message(Message#'ExecutionReport'{'TimeInForce' = Value}, Fields);
	{'ExecRestatementReason', Value} -> parse_message(Message#'ExecutionReport'{'ExecRestatementReason' = Value}, Fields);
	{'OrdRejReason', Value} -> parse_message(Message#'ExecutionReport'{'OrdRejReason' = Value}, Fields);
	{'Text', Value} -> parse_message(Message#'ExecutionReport'{'Text' = Value}, Fields);
	{'Symbol', Value} -> parse_message(Message#'ExecutionReport'{'Symbol' = Value}, Fields);
	{'Side', Value} -> parse_message(Message#'ExecutionReport'{'Side' = Value}, Fields);
	{'SecurityID', Value} -> parse_message(Message#'ExecutionReport'{'SecurityID' = Value}, Fields);
	{'Price', Value} -> parse_message(Message#'ExecutionReport'{'Price' = Value}, Fields);
	{'OrigClOrdID', Value} -> parse_message(Message#'ExecutionReport'{'OrigClOrdID' = Value}, Fields);
	{'OrdType', Value} -> parse_message(Message#'ExecutionReport'{'OrdType' = Value}, Fields);
	{'OrdStatus', Value} -> parse_message(Message#'ExecutionReport'{'OrdStatus' = Value}, Fields);
	{'OrderQty', Value} -> parse_message(Message#'ExecutionReport'{'OrderQty' = Value}, Fields);
	{'SecondaryOrderID', Value} -> parse_message(Message#'ExecutionReport'{'SecondaryOrderID' = Value}, Fields);
	{'OrderID', Value} -> parse_message(Message#'ExecutionReport'{'OrderID' = Value}, Fields);
	{'LastShares', Value} -> parse_message(Message#'ExecutionReport'{'LastShares' = Value}, Fields);
	{'LastPx', Value} -> parse_message(Message#'ExecutionReport'{'LastPx' = Value}, Fields);
	{'ExecTransType', Value} -> parse_message(Message#'ExecutionReport'{'ExecTransType' = Value}, Fields);
	{'ExecRefID', Value} -> parse_message(Message#'ExecutionReport'{'ExecRefID' = Value}, Fields);
	{'ExecInst', Value} -> parse_message(Message#'ExecutionReport'{'ExecInst' = Value}, Fields);
	{'ExecID', Value} -> parse_message(Message#'ExecutionReport'{'ExecID' = Value}, Fields);
	{'Currency', Value} -> parse_message(Message#'ExecutionReport'{'Currency' = Value}, Fields);
	{'CumQty', Value} -> parse_message(Message#'ExecutionReport'{'CumQty' = Value}, Fields);
	{'ClOrdID', Value} -> parse_message(Message#'ExecutionReport'{'ClOrdID' = Value}, Fields);
	{'AvgPx', Value} -> parse_message(Message#'ExecutionReport'{'AvgPx' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'OrderCancelReplaceRequest'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'ClRefID', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'ClRefID' = Value}, Fields);
	{'BrSeqNbr', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'BrSeqNbr' = Value}, Fields);
	{'SubMktID', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'SubMktID' = Value}, Fields);
	{'ClearingAccount', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'ClearingAccount' = Value}, Fields);
	{'ClearingFirm', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'ClearingFirm' = Value}, Fields);
	{'ExpireTime', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'ExpireTime' = Value}, Fields);
	{'MaxFloor', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'MaxFloor' = Value}, Fields);
	{'TransactTime', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'TransactTime' = Value}, Fields);
	{'TimeInForce', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'TimeInForce' = Value}, Fields);
	{'Text', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'Text' = Value}, Fields);
	{'Symbol', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'Symbol' = Value}, Fields);
	{'Side', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'Side' = Value}, Fields);
	{'SecurityID', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'SecurityID' = Value}, Fields);
	{'Price', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'Price' = Value}, Fields);
	{'OrigClOrdID', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'OrigClOrdID' = Value}, Fields);
	{'OrdType', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'OrdType' = Value}, Fields);
	{'OrderQty', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'OrderQty' = Value}, Fields);
	{'OrderID', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'OrderID' = Value}, Fields);
	{'HandlInst', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'HandlInst' = Value}, Fields);
	{'Currency', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'Currency' = Value}, Fields);
	{'ClOrdID', Value} -> parse_message(Message#'OrderCancelReplaceRequest'{'ClOrdID' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'OrderCancelRequest'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'SubMktID', Value} -> parse_message(Message#'OrderCancelRequest'{'SubMktID' = Value}, Fields);
	{'TransactTime', Value} -> parse_message(Message#'OrderCancelRequest'{'TransactTime' = Value}, Fields);
	{'Symbol', Value} -> parse_message(Message#'OrderCancelRequest'{'Symbol' = Value}, Fields);
	{'Side', Value} -> parse_message(Message#'OrderCancelRequest'{'Side' = Value}, Fields);
	{'SecurityID', Value} -> parse_message(Message#'OrderCancelRequest'{'SecurityID' = Value}, Fields);
	{'OrigClOrdID', Value} -> parse_message(Message#'OrderCancelRequest'{'OrigClOrdID' = Value}, Fields);
	{'OrderQty', Value} -> parse_message(Message#'OrderCancelRequest'{'OrderQty' = Value}, Fields);
	{'OrderID', Value} -> parse_message(Message#'OrderCancelRequest'{'OrderID' = Value}, Fields);
	{'Currency', Value} -> parse_message(Message#'OrderCancelRequest'{'Currency' = Value}, Fields);
	{'ClOrdID', Value} -> parse_message(Message#'OrderCancelRequest'{'ClOrdID' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end;
parse_message(Message = #'OrderCancelReject'{}, [Field|Fields]) ->
    case parse_field(Field) of
	{'CxlRejResponseTo', Value} -> parse_message(Message#'OrderCancelReject'{'CxlRejResponseTo' = Value}, Fields);
	{'ClientID', Value} -> parse_message(Message#'OrderCancelReject'{'ClientID' = Value}, Fields);
	{'CxlRejReason', Value} -> parse_message(Message#'OrderCancelReject'{'CxlRejReason' = Value}, Fields);
	{'Text', Value} -> parse_message(Message#'OrderCancelReject'{'Text' = Value}, Fields);
	{'OrigClOrdID', Value} -> parse_message(Message#'OrderCancelReject'{'OrigClOrdID' = Value}, Fields);
	{'OrdStatus', Value} -> parse_message(Message#'OrderCancelReject'{'OrdStatus' = Value}, Fields);
	{'OrderID', Value} -> parse_message(Message#'OrderCancelReject'{'OrderID' = Value}, Fields);
	{'ClOrdID', Value} -> parse_message(Message#'OrderCancelReject'{'ClOrdID' = Value}, Fields);
	_ -> {Message, [Field|Fields]}
    end.


parse_field(<<"1=", Value/binary>>) -> {'Account', Value};
parse_field(<<"2=", Value/binary>>) -> {'AdvId', Value};
parse_field(<<"3=", Value/binary>>) -> {'AdvRefID', Value};
parse_field(<<"4=", Value/binary>>) ->
    case Value of
	<<"B">> -> {'AdvSide','BUY'};
	<<"S">> -> {'AdvSide','SELL'};
	<<"X">> -> {'AdvSide','CROSS'};
	<<"T">> -> {'AdvSide','TRADE'};
	_ -> {'AdvSide',Value}
    end;
parse_field(<<"5=", Value/binary>>) ->
    case Value of
	<<"N">> -> {'AdvTransType','NEW'};
	<<"C">> -> {'AdvTransType','CANCEL'};
	<<"R">> -> {'AdvTransType','REPLACE'};
	_ -> {'AdvTransType',Value}
    end;
parse_field(<<"6=", Value/binary>>) -> {'AvgPx', Value};
parse_field(<<"7=", Value/binary>>) -> {'BeginSeqNo', binary_to_int(Value)};
parse_field(<<"8=", Value/binary>>) -> {'BeginString', Value};
parse_field(<<"9=", Value/binary>>) -> {'BodyLength', binary_to_int(Value)};
parse_field(<<"10=", Value/binary>>) -> {'CheckSum', binary_to_int(Value)};
parse_field(<<"11=", Value/binary>>) -> {'ClOrdID', Value};
parse_field(<<"12=", Value/binary>>) -> {'Commission', Value};
parse_field(<<"13=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'CommType','PER_SHARE'};
	<<"2">> -> {'CommType','PERCENTAGE'};
	<<"3">> -> {'CommType','ABSOLUTE'};
	_ -> {'CommType',Value}
    end;
parse_field(<<"14=", Value/binary>>) -> {'CumQty', Value};
parse_field(<<"15=", Value/binary>>) -> {'Currency', Value};
parse_field(<<"16=", Value/binary>>) -> {'EndSeqNo', binary_to_int(Value)};
parse_field(<<"17=", Value/binary>>) -> {'ExecID', Value};
parse_field(<<"18=", Value/binary>>) ->
    case Value of
	<<"M">> -> {'ExecInst','MIDPRICE_PEG'};
	<<"N">> -> {'ExecInst','NONNEGOTIABLE'};
	<<"P">> -> {'ExecInst','MARKET_PEG'};
	<<"R">> -> {'ExecInst','PRIMARY_PEG'};
	_ -> {'ExecInst',Value}
    end;
parse_field(<<"19=", Value/binary>>) -> {'ExecRefID', Value};
parse_field(<<"20=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'ExecTransType','NEW'};
	<<"1">> -> {'ExecTransType','CANCEL'};
	<<"2">> -> {'ExecTransType','CORRECT'};
	<<"3">> -> {'ExecTransType','STATUS'};
	_ -> {'ExecTransType',Value}
    end;
parse_field(<<"21=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'HandlInst','AUTOMATED_EXECUTION_ORDER_PRIVATE_NO_BROKER_INTERVENTION'};
	<<"2">> -> {'HandlInst','AUTOMATED_EXECUTION_ORDER_PUBLIC_BROKER_INTERVENTION_OK'};
	<<"3">> -> {'HandlInst','MANUAL_ORDER_BEST_EXECUTION'};
	_ -> {'HandlInst',Value}
    end;
parse_field(<<"22=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'IDSource','CUSIP'};
	<<"2">> -> {'IDSource','SEDOL'};
	<<"3">> -> {'IDSource','QUIK'};
	<<"4">> -> {'IDSource','ISIN_NUMBER'};
	<<"5">> -> {'IDSource','RIC_CODE'};
	<<"6">> -> {'IDSource','ISO_CURRENCY_CODE'};
	<<"7">> -> {'IDSource','ISO_COUNTRY_CODE'};
	<<"8">> -> {'IDSource','EXCHANGE_SYMBOL'};
	<<"9">> -> {'IDSource','CONSOLIDATED_TAPE_ASSOCIATION'};
	_ -> {'IDSource',Value}
    end;
parse_field(<<"23=", Value/binary>>) -> {'IOIid', Value};
parse_field(<<"24=", Value/binary>>) -> {'IOIOthSvc', Value};
parse_field(<<"25=", Value/binary>>) ->
    case Value of
	<<"L">> -> {'IOIQltyInd','LOW'};
	<<"M">> -> {'IOIQltyInd','MEDIUM'};
	<<"H">> -> {'IOIQltyInd','HIGH'};
	_ -> {'IOIQltyInd',Value}
    end;
parse_field(<<"26=", Value/binary>>) -> {'IOIRefID', Value};
parse_field(<<"27=", Value/binary>>) -> {'IOIShares', Value};
parse_field(<<"28=", Value/binary>>) ->
    case Value of
	<<"N">> -> {'IOITransType','NEW'};
	<<"C">> -> {'IOITransType','CANCEL'};
	<<"R">> -> {'IOITransType','REPLACE'};
	_ -> {'IOITransType',Value}
    end;
parse_field(<<"29=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'LastCapacity','AGENT'};
	<<"2">> -> {'LastCapacity','CROSS_AS_AGENT'};
	<<"3">> -> {'LastCapacity','CROSS_AS_PRINCIPAL'};
	<<"4">> -> {'LastCapacity','PRINCIPAL'};
	_ -> {'LastCapacity',Value}
    end;
parse_field(<<"30=", Value/binary>>) -> {'LastMkt', Value};
parse_field(<<"31=", Value/binary>>) -> {'LastPx', Value};
parse_field(<<"32=", Value/binary>>) -> {'LastShares', Value};
parse_field(<<"33=", Value/binary>>) -> {'LinesOfText', binary_to_int(Value)};
parse_field(<<"34=", Value/binary>>) -> {'MsgSeqNum', binary_to_int(Value)};
parse_field(<<"35=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'MsgType','HEARTBEAT'};
	<<"1">> -> {'MsgType','TEST_REQUEST'};
	<<"2">> -> {'MsgType','RESEND_REQUEST'};
	<<"3">> -> {'MsgType','REJECT'};
	<<"4">> -> {'MsgType','SEQUENCE_RESET'};
	<<"5">> -> {'MsgType','LOGOUT'};
	<<"6">> -> {'MsgType','INDICATION_OF_INTEREST'};
	<<"7">> -> {'MsgType','ADVERTISEMENT'};
	<<"8">> -> {'MsgType','EXECUTION_REPORT'};
	<<"9">> -> {'MsgType','ORDER_CANCEL_REJECT'};
	<<"a">> -> {'MsgType','QUOTE_STATUS_REQUEST'};
	<<"A">> -> {'MsgType','LOGON'};
	<<"B">> -> {'MsgType','NEWS'};
	<<"b">> -> {'MsgType','QUOTE_ACKNOWLEDGEMENT'};
	<<"C">> -> {'MsgType','EMAIL'};
	<<"c">> -> {'MsgType','SECURITY_DEFINITION_REQUEST'};
	<<"D">> -> {'MsgType','ORDER_SINGLE'};
	<<"d">> -> {'MsgType','SECURITY_DEFINITION'};
	<<"E">> -> {'MsgType','ORDER_LIST'};
	<<"e">> -> {'MsgType','SECURITY_STATUS_REQUEST'};
	<<"f">> -> {'MsgType','SECURITY_STATUS'};
	<<"F">> -> {'MsgType','ORDER_CANCEL_REQUEST'};
	<<"G">> -> {'MsgType','ORDER_CANCEL_REPLACE_REQUEST'};
	<<"g">> -> {'MsgType','TRADING_SESSION_STATUS_REQUEST'};
	<<"H">> -> {'MsgType','ORDER_STATUS_REQUEST'};
	<<"h">> -> {'MsgType','TRADING_SESSION_STATUS'};
	<<"i">> -> {'MsgType','MASS_QUOTE'};
	<<"j">> -> {'MsgType','BUSINESS_MESSAGE_REJECT'};
	<<"J">> -> {'MsgType','ALLOCATION'};
	<<"K">> -> {'MsgType','LIST_CANCEL_REQUEST'};
	<<"k">> -> {'MsgType','BID_REQUEST'};
	<<"l">> -> {'MsgType','BID_RESPONSE'};
	<<"L">> -> {'MsgType','LIST_EXECUTE'};
	<<"m">> -> {'MsgType','LIST_STRIKE_PRICE'};
	<<"M">> -> {'MsgType','LIST_STATUS_REQUEST'};
	<<"N">> -> {'MsgType','LIST_STATUS'};
	<<"P">> -> {'MsgType','ALLOCATION_ACK'};
	<<"Q">> -> {'MsgType','DONT_KNOW_TRADE'};
	<<"R">> -> {'MsgType','QUOTE_REQUEST'};
	<<"S">> -> {'MsgType','QUOTE'};
	<<"T">> -> {'MsgType','SETTLEMENT_INSTRUCTIONS'};
	<<"V">> -> {'MsgType','MARKET_DATA_REQUEST'};
	<<"W">> -> {'MsgType','MARKET_DATA_SNAPSHOT_FULL_REFRESH'};
	<<"X">> -> {'MsgType','MARKET_DATA_INCREMENTAL_REFRESH'};
	<<"Y">> -> {'MsgType','MARKET_DATA_REQUEST_REJECT'};
	<<"Z">> -> {'MsgType','QUOTE_CANCEL'};
	_ -> {'MsgType',Value}
    end;
parse_field(<<"36=", Value/binary>>) -> {'NewSeqNo', binary_to_int(Value)};
parse_field(<<"37=", Value/binary>>) -> {'OrderID', Value};
parse_field(<<"38=", Value/binary>>) -> {'OrderQty', Value};
parse_field(<<"39=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'OrdStatus','NEW'};
	<<"1">> -> {'OrdStatus','PARTIALLY_FILLED'};
	<<"2">> -> {'OrdStatus','FILLED'};
	<<"3">> -> {'OrdStatus','DONE_FOR_DAY'};
	<<"4">> -> {'OrdStatus','CANCELED'};
	<<"5">> -> {'OrdStatus','REPLACED'};
	<<"6">> -> {'OrdStatus','PENDING_CANCEL'};
	<<"7">> -> {'OrdStatus','STOPPED'};
	<<"8">> -> {'OrdStatus','REJECTED'};
	<<"9">> -> {'OrdStatus','SUSPENDED'};
	<<"A">> -> {'OrdStatus','PENDING_NEW'};
	<<"B">> -> {'OrdStatus','CALCULATED'};
	<<"C">> -> {'OrdStatus','EXPIRED'};
	<<"D">> -> {'OrdStatus','ACCEPTED_FOR_BIDDING'};
	<<"E">> -> {'OrdStatus','PENDING_REPLACE'};
	_ -> {'OrdStatus',Value}
    end;
parse_field(<<"40=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'OrdType','MARKET'};
	<<"2">> -> {'OrdType','LIMIT'};
	<<"P">> -> {'OrdType','PEGGED'};
	_ -> {'OrdType',Value}
    end;
parse_field(<<"41=", Value/binary>>) -> {'OrigClOrdID', Value};
parse_field(<<"42=", Value/binary>>) -> {'OrigTime', Value};
parse_field(<<"43=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'PossDupFlag','POSSIBLE_DUPLICATE'};
	<<"N">> -> {'PossDupFlag','ORIGINAL_TRANSMISSION'};
	_ -> {'PossDupFlag',Value}
    end;
parse_field(<<"44=", Value/binary>>) -> {'Price', Value};
parse_field(<<"45=", Value/binary>>) -> {'RefSeqNum', binary_to_int(Value)};
parse_field(<<"46=", Value/binary>>) -> {'RelatdSym', Value};
parse_field(<<"47=", Value/binary>>) ->
    case Value of
	<<"A">> -> {'Rule80A','AGENCY_SINGLE_ORDER'};
	<<"B">> -> {'Rule80A','SHORT_EXEMPT_TRANSACTION_B'};
	<<"C">> -> {'Rule80A','PROGRAM_ORDER_NONINDEX_ARB_FOR_MEMBER_FIRMORG'};
	<<"D">> -> {'Rule80A','PROGRAM_ORDER_INDEX_ARB_FOR_MEMBER_FIRMORG'};
	<<"E">> -> {'Rule80A','REGISTERED_EQUITY_MARKET_MAKER_TRADES'};
	<<"F">> -> {'Rule80A','SHORT_EXEMPT_TRANSACTION_F'};
	<<"H">> -> {'Rule80A','SHORT_EXEMPT_TRANSACTION_H'};
	<<"J">> -> {'Rule80A','PROGRAM_ORDER_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER'};
	<<"K">> -> {'Rule80A','PROGRAM_ORDER_NONINDEX_ARB_FOR_INDIVIDUAL_CUSTOMER'};
	<<"L">> -> {'Rule80A','SHORT_EXEMPT_AFFILIATED'};
	<<"M">> -> {'Rule80A','PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_MEMBER'};
	<<"N">> -> {'Rule80A','PROGRAM_ORDER_NONINDEX_ARB_FOR_OTHER_MEMBER'};
	<<"O">> -> {'Rule80A','COMPETING_DEALER_TRADES_O'};
	<<"P">> -> {'Rule80A','PRINCIPAL'};
	<<"R">> -> {'Rule80A','COMPETING_DEALER_TRADES_R'};
	<<"S">> -> {'Rule80A','SPECIALIST_TRADES'};
	<<"T">> -> {'Rule80A','COMPETING_DEALER_TRADES_T'};
	<<"U">> -> {'Rule80A','PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_AGENCY'};
	<<"W">> -> {'Rule80A','ALL_OTHER_ORDERS_AS_AGENT_FOR_OTHER_MEMBER'};
	<<"X">> -> {'Rule80A','SHORT_EXEMPT_NOT_AFFILIATED'};
	<<"Y">> -> {'Rule80A','PROGRAM_ORDER_NONINDEX_ARB_FOR_OTHER_AGENCY'};
	<<"Z">> -> {'Rule80A','SHORT_EXEMPT_NONMEMBER'};
	_ -> {'Rule80A',Value}
    end;
parse_field(<<"48=", Value/binary>>) -> {'SecurityID', Value};
parse_field(<<"49=", Value/binary>>) ->
    case Value of
	<<"INORD">> -> {'SenderCompID','INORD'};
	_ -> {'SenderCompID',Value}
    end;
parse_field(<<"50=", Value/binary>>) -> {'SenderSubID', Value};
parse_field(<<"52=", Value/binary>>) -> {'SendingTime', Value};
parse_field(<<"53=", Value/binary>>) -> {'Shares', Value};
parse_field(<<"54=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'Side','BUY'};
	<<"2">> -> {'Side','SELL'};
	<<"7">> -> {'Side','UNDISCLOSED'};
	<<"8">> -> {'Side','CROSS'};
	_ -> {'Side',Value}
    end;
parse_field(<<"55=", Value/binary>>) -> {'Symbol', Value};
parse_field(<<"56=", Value/binary>>) -> {'TargetCompID', Value};
parse_field(<<"57=", Value/binary>>) ->
    case Value of
	<<"T">> -> {'TargetSubID','TRADE_REPORT'};
	<<"S">> -> {'TargetSubID','ORDER_ENTRY'};
	_ -> {'TargetSubID',Value}
    end;
parse_field(<<"58=", Value/binary>>) -> {'Text', Value};
parse_field(<<"59=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'TimeInForce','DAY'};
	<<"1">> -> {'TimeInForce','GOOD_TILL_CANCEL'};
	<<"2">> -> {'TimeInForce','AT_THE_OPENING'};
	<<"3">> -> {'TimeInForce','IMMEDIATE_OR_CANCEL'};
	<<"4">> -> {'TimeInForce','FILL_OR_KILL'};
	<<"6">> -> {'TimeInForce','GOOD_TILL_TIME'};
	<<"7">> -> {'TimeInForce','AT_THE_CLOSE'};
	<<"9">> -> {'TimeInForce','GOOD_TIL_NEXT_CROSS'};
	_ -> {'TimeInForce',Value}
    end;
parse_field(<<"60=", Value/binary>>) -> {'TransactTime', Value};
parse_field(<<"61=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'Urgency','NORMAL'};
	<<"1">> -> {'Urgency','FLASH'};
	<<"2">> -> {'Urgency','BACKGROUND'};
	_ -> {'Urgency',Value}
    end;
parse_field(<<"62=", Value/binary>>) -> {'ValidUntilTime', Value};
parse_field(<<"63=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'SettlmntTyp','REGULAR'};
	<<"1">> -> {'SettlmntTyp','CASH'};
	<<"2">> -> {'SettlmntTyp','NEXT_DAY'};
	<<"3">> -> {'SettlmntTyp','TPLUS2'};
	<<"4">> -> {'SettlmntTyp','TPLUS3'};
	<<"5">> -> {'SettlmntTyp','TPLUS4'};
	<<"6">> -> {'SettlmntTyp','FUTURE'};
	<<"7">> -> {'SettlmntTyp','WHEN_ISSUED'};
	<<"8">> -> {'SettlmntTyp','SELLERS_OPTION'};
	<<"9">> -> {'SettlmntTyp','TPLUS5'};
	_ -> {'SettlmntTyp',Value}
    end;
parse_field(<<"64=", Value/binary>>) -> {'FutSettDate', Value};
parse_field(<<"65=", Value/binary>>) -> {'SymbolSfx', Value};
parse_field(<<"66=", Value/binary>>) -> {'ListID', Value};
parse_field(<<"67=", Value/binary>>) -> {'ListSeqNo', binary_to_int(Value)};
parse_field(<<"68=", Value/binary>>) -> {'TotNoOrders', binary_to_int(Value)};
parse_field(<<"69=", Value/binary>>) -> {'ListExecInst', Value};
parse_field(<<"70=", Value/binary>>) -> {'AllocID', Value};
parse_field(<<"71=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'AllocTransType','NEW'};
	<<"1">> -> {'AllocTransType','REPLACE'};
	<<"2">> -> {'AllocTransType','CANCEL'};
	<<"3">> -> {'AllocTransType','PRELIMINARY'};
	<<"4">> -> {'AllocTransType','CALCULATED'};
	<<"5">> -> {'AllocTransType','CALCULATED_WITHOUT_PRELIMINARY'};
	_ -> {'AllocTransType',Value}
    end;
parse_field(<<"72=", Value/binary>>) -> {'RefAllocID', Value};
parse_field(<<"73=", Value/binary>>) -> {'NoOrders', binary_to_int(Value)};
parse_field(<<"74=", Value/binary>>) -> {'AvgPrxPrecision', binary_to_int(Value)};
parse_field(<<"75=", Value/binary>>) -> {'TradeDate', Value};
parse_field(<<"76=", Value/binary>>) ->
    case Value of
	<<"BOOK">> -> {'ExecBroker','BOOK'};
	<<"SCAN">> -> {'ExecBroker','SCAN'};
	<<"STGY">> -> {'ExecBroker','STGY'};
	_ -> {'ExecBroker',Value}
    end;
parse_field(<<"77=", Value/binary>>) ->
    case Value of
	<<"O">> -> {'OpenClose','OPEN'};
	<<"C">> -> {'OpenClose','CLOSE'};
	_ -> {'OpenClose',Value}
    end;
parse_field(<<"78=", Value/binary>>) -> {'NoAllocs', binary_to_int(Value)};
parse_field(<<"79=", Value/binary>>) -> {'AllocAccount', Value};
parse_field(<<"80=", Value/binary>>) -> {'AllocShares', Value};
parse_field(<<"81=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'ProcessCode','REGULAR'};
	<<"1">> -> {'ProcessCode','SOFT_DOLLAR'};
	<<"2">> -> {'ProcessCode','STEPIN'};
	<<"3">> -> {'ProcessCode','STEPOUT'};
	<<"4">> -> {'ProcessCode','SOFTDOLLAR_STEPIN'};
	<<"5">> -> {'ProcessCode','SOFTDOLLAR_STEPOUT'};
	<<"6">> -> {'ProcessCode','PLAN_SPONSOR'};
	_ -> {'ProcessCode',Value}
    end;
parse_field(<<"82=", Value/binary>>) -> {'NoRpts', binary_to_int(Value)};
parse_field(<<"83=", Value/binary>>) -> {'RptSeq', binary_to_int(Value)};
parse_field(<<"84=", Value/binary>>) -> {'CxlQty', Value};
parse_field(<<"85=", Value/binary>>) -> {'NoDlvyInst', binary_to_int(Value)};
parse_field(<<"86=", Value/binary>>) -> {'DlvyInst', Value};
parse_field(<<"87=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'AllocStatus','ACCEPTED'};
	<<"1">> -> {'AllocStatus','REJECTED'};
	<<"2">> -> {'AllocStatus','PARTIAL_ACCEPT'};
	<<"3">> -> {'AllocStatus','RECEIVED'};
	_ -> {'AllocStatus',Value}
    end;
parse_field(<<"88=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'AllocRejCode','UNKNOWN_ACCOUNT'};
	<<"1">> -> {'AllocRejCode','INCORRECT_QUANTITY'};
	<<"2">> -> {'AllocRejCode','INCORRECT_AVERAGE_PRICE'};
	<<"3">> -> {'AllocRejCode','UNKNOWN_EXECUTING_BROKER_MNEMONIC'};
	<<"4">> -> {'AllocRejCode','COMMISSION_DIFFERENCE'};
	<<"5">> -> {'AllocRejCode','UNKNOWN_ORDERID'};
	<<"6">> -> {'AllocRejCode','UNKNOWN_LISTID'};
	<<"7">> -> {'AllocRejCode','OTHER'};
	_ -> {'AllocRejCode',Value}
    end;
parse_field(<<"89=", Value/binary>>) -> {'Signature', Value};
parse_field(<<"90=", Value/binary>>) -> {'SecureDataLen', binary_to_int(Value)};
parse_field(<<"91=", Value/binary>>) -> {'SecureData', Value};
parse_field(<<"92=", Value/binary>>) -> {'BrokerOfCredit', Value};
parse_field(<<"93=", Value/binary>>) -> {'SignatureLength', binary_to_int(Value)};
parse_field(<<"94=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'EmailType','NEW'};
	<<"1">> -> {'EmailType','REPLY'};
	<<"2">> -> {'EmailType','ADMIN_REPLY'};
	_ -> {'EmailType',Value}
    end;
parse_field(<<"95=", Value/binary>>) -> {'RawDataLength', binary_to_int(Value)};
parse_field(<<"96=", Value/binary>>) -> {'RawData', Value};
parse_field(<<"97=", Value/binary>>) -> {'PossResend', Value};
parse_field(<<"98=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'EncryptMethod','NONE_OTHER'};
	_ -> {'EncryptMethod',Value}
    end;
parse_field(<<"99=", Value/binary>>) -> {'StopPx', Value};
parse_field(<<"100=", Value/binary>>) -> {'ExDestination', Value};
parse_field(<<"102=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'CxlRejReason','TOO_LATE_TO_CANCEL'};
	<<"1">> -> {'CxlRejReason','UNKNOWN_ORDER'};
	<<"2">> -> {'CxlRejReason','BROKER_OPTION'};
	<<"3">> -> {'CxlRejReason','ALREADY_PENDING'};
	_ -> {'CxlRejReason',Value}
    end;
parse_field(<<"103=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'OrdRejReason','BROKER_OPTION'};
	<<"1">> -> {'OrdRejReason','UNKNOWN_SYMBOL'};
	<<"2">> -> {'OrdRejReason','EXCHANGE_CLOSED'};
	<<"3">> -> {'OrdRejReason','ORDER_EXCEEDS_LIMIT'};
	<<"4">> -> {'OrdRejReason','TOO_LATE_TO_ENTER'};
	<<"5">> -> {'OrdRejReason','UNKNOWN_ORDER'};
	<<"6">> -> {'OrdRejReason','DUPLICATE_ORDER'};
	<<"7">> -> {'OrdRejReason','DUPLICATE_VERBALYES'};
	<<"8">> -> {'OrdRejReason','STALE_ORDER'};
	_ -> {'OrdRejReason',Value}
    end;
parse_field(<<"104=", Value/binary>>) ->
    case Value of
	<<"A">> -> {'IOIQualifier','ALL_OR_NONE'};
	<<"C">> -> {'IOIQualifier','AT_THE_CLOSE'};
	<<"I">> -> {'IOIQualifier','IN_TOUCH_WITH'};
	<<"L">> -> {'IOIQualifier','LIMIT'};
	<<"M">> -> {'IOIQualifier','MORE_BEHIND'};
	<<"O">> -> {'IOIQualifier','AT_THE_OPEN'};
	<<"P">> -> {'IOIQualifier','TAKING_A_POSITION'};
	<<"Q">> -> {'IOIQualifier','AT_THE_MARKET'};
	<<"R">> -> {'IOIQualifier','READY_TO_TRADE'};
	<<"S">> -> {'IOIQualifier','PORTFOLIO_SHOWN'};
	<<"T">> -> {'IOIQualifier','THROUGH_THE_DAY'};
	<<"V">> -> {'IOIQualifier','VERSUS'};
	<<"W">> -> {'IOIQualifier','INDICATION_WORKING_AWAY'};
	<<"X">> -> {'IOIQualifier','CROSSING_OPPORTUNITY'};
	<<"Y">> -> {'IOIQualifier','AT_THE_MIDPOINT'};
	<<"Z">> -> {'IOIQualifier','PREOPEN'};
	_ -> {'IOIQualifier',Value}
    end;
parse_field(<<"105=", Value/binary>>) -> {'WaveNo', Value};
parse_field(<<"106=", Value/binary>>) -> {'Issuer', Value};
parse_field(<<"107=", Value/binary>>) -> {'SecurityDesc', Value};
parse_field(<<"108=", Value/binary>>) -> {'HeartBtInt', binary_to_int(Value)};
parse_field(<<"109=", Value/binary>>) -> {'ClientID', Value};
parse_field(<<"110=", Value/binary>>) -> {'MinQty', Value};
parse_field(<<"111=", Value/binary>>) -> {'MaxFloor', Value};
parse_field(<<"112=", Value/binary>>) -> {'TestReqID', Value};
parse_field(<<"113=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'ReportToExch','YES'};
	<<"N">> -> {'ReportToExch','NO'};
	_ -> {'ReportToExch',Value}
    end;
parse_field(<<"114=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'LocateReqd','YES'};
	<<"N">> -> {'LocateReqd','NO'};
	_ -> {'LocateReqd',Value}
    end;
parse_field(<<"115=", Value/binary>>) -> {'OnBehalfOfCompID', Value};
parse_field(<<"116=", Value/binary>>) -> {'OnBehalfOfSubID', Value};
parse_field(<<"117=", Value/binary>>) -> {'QuoteID', Value};
parse_field(<<"118=", Value/binary>>) -> {'NetMoney', Value};
parse_field(<<"119=", Value/binary>>) -> {'SettlCurrAmt', Value};
parse_field(<<"120=", Value/binary>>) -> {'SettlCurrency', Value};
parse_field(<<"121=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'ForexReq','YES'};
	<<"N">> -> {'ForexReq','NO'};
	_ -> {'ForexReq',Value}
    end;
parse_field(<<"122=", Value/binary>>) -> {'OrigSendingTime', Value};
parse_field(<<"123=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'GapFillFlag','GAP_FILL_MESSAGE_MSGSEQNUM_FIELD_VALID'};
	<<"N">> -> {'GapFillFlag','SEQUENCE_RESET_IGNORE_MSGSEQNUM'};
	_ -> {'GapFillFlag',Value}
    end;
parse_field(<<"124=", Value/binary>>) -> {'NoExecs', binary_to_int(Value)};
parse_field(<<"125=", Value/binary>>) -> {'CxlType', Value};
parse_field(<<"126=", Value/binary>>) -> {'ExpireTime', Value};
parse_field(<<"127=", Value/binary>>) ->
    case Value of
	<<"A">> -> {'DKReason','UNKNOWN_SYMBOL'};
	<<"B">> -> {'DKReason','WRONG_SIDE'};
	<<"C">> -> {'DKReason','QUANTITY_EXCEEDS_ORDER'};
	<<"D">> -> {'DKReason','NO_MATCHING_ORDER'};
	<<"E">> -> {'DKReason','PRICE_EXCEEDS_LIMIT'};
	<<"Z">> -> {'DKReason','OTHER'};
	_ -> {'DKReason',Value}
    end;
parse_field(<<"128=", Value/binary>>) -> {'DeliverToCompID', Value};
parse_field(<<"129=", Value/binary>>) -> {'DeliverToSubID', Value};
parse_field(<<"130=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'IOINaturalFlag','NATURAL'};
	<<"N">> -> {'IOINaturalFlag','NOT_NATURAL'};
	_ -> {'IOINaturalFlag',Value}
    end;
parse_field(<<"131=", Value/binary>>) -> {'QuoteReqID', Value};
parse_field(<<"132=", Value/binary>>) -> {'BidPx', Value};
parse_field(<<"133=", Value/binary>>) -> {'OfferPx', Value};
parse_field(<<"134=", Value/binary>>) -> {'BidSize', Value};
parse_field(<<"135=", Value/binary>>) -> {'OfferSize', Value};
parse_field(<<"136=", Value/binary>>) -> {'NoMiscFees', binary_to_int(Value)};
parse_field(<<"137=", Value/binary>>) -> {'MiscFeeAmt', Value};
parse_field(<<"138=", Value/binary>>) -> {'MiscFeeCurr', Value};
parse_field(<<"139=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'MiscFeeType','REGULATORY'};
	<<"2">> -> {'MiscFeeType','TAX'};
	<<"3">> -> {'MiscFeeType','LOCAL_COMMISSION'};
	<<"4">> -> {'MiscFeeType','EXCHANGE_FEES'};
	<<"5">> -> {'MiscFeeType','STAMP'};
	<<"6">> -> {'MiscFeeType','LEVY'};
	<<"7">> -> {'MiscFeeType','OTHER'};
	<<"8">> -> {'MiscFeeType','MARKUP'};
	<<"9">> -> {'MiscFeeType','CONSUMPTION_TAX'};
	_ -> {'MiscFeeType',Value}
    end;
parse_field(<<"140=", Value/binary>>) -> {'PrevClosePx', Value};
parse_field(<<"141=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'ResetSeqNumFlag','YES_RESET_SEQUENCE_NUMBERS'};
	<<"N">> -> {'ResetSeqNumFlag','NO'};
	_ -> {'ResetSeqNumFlag',Value}
    end;
parse_field(<<"142=", Value/binary>>) -> {'SenderLocationID', Value};
parse_field(<<"143=", Value/binary>>) -> {'TargetLocationID', Value};
parse_field(<<"144=", Value/binary>>) -> {'OnBehalfOfLocationID', Value};
parse_field(<<"145=", Value/binary>>) -> {'DeliverToLocationID', Value};
parse_field(<<"146=", Value/binary>>) -> {'NoRelatedSym', binary_to_int(Value)};
parse_field(<<"147=", Value/binary>>) -> {'Subject', Value};
parse_field(<<"148=", Value/binary>>) -> {'Headline', Value};
parse_field(<<"149=", Value/binary>>) -> {'URLLink', Value};
parse_field(<<"150=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'ExecType','NEW'};
	<<"1">> -> {'ExecType','PARTIAL_FILL'};
	<<"2">> -> {'ExecType','FILL'};
	<<"3">> -> {'ExecType','DONE_FOR_DAY'};
	<<"4">> -> {'ExecType','CANCELED'};
	<<"5">> -> {'ExecType','REPLACE'};
	<<"6">> -> {'ExecType','PENDING_CANCEL'};
	<<"7">> -> {'ExecType','STOPPED'};
	<<"8">> -> {'ExecType','REJECTED'};
	<<"9">> -> {'ExecType','SUSPENDED'};
	<<"A">> -> {'ExecType','PENDING_NEW'};
	<<"B">> -> {'ExecType','CALCULATED'};
	<<"C">> -> {'ExecType','EXPIRED'};
	<<"D">> -> {'ExecType','RESTATED'};
	<<"E">> -> {'ExecType','PENDING_REPLACE'};
	<<"F">> -> {'ExecType','TRADE_REPORT'};
	<<"I">> -> {'ExecType','INFORMATION'};
	_ -> {'ExecType',Value}
    end;
parse_field(<<"151=", Value/binary>>) -> {'LeavesQty', Value};
parse_field(<<"152=", Value/binary>>) -> {'CashOrderQty', Value};
parse_field(<<"153=", Value/binary>>) -> {'AllocAvgPx', Value};
parse_field(<<"154=", Value/binary>>) -> {'AllocNetMoney', Value};
parse_field(<<"155=", Value/binary>>) -> {'SettlCurrFxRate', Value};
parse_field(<<"156=", Value/binary>>) ->
    case Value of
	<<"M">> -> {'SettlCurrFxRateCalc','MULTIPLY'};
	<<"D">> -> {'SettlCurrFxRateCalc','DIVIDE'};
	_ -> {'SettlCurrFxRateCalc',Value}
    end;
parse_field(<<"157=", Value/binary>>) -> {'NumDaysInterest', binary_to_int(Value)};
parse_field(<<"158=", Value/binary>>) -> {'AccruedInterestRate', Value};
parse_field(<<"159=", Value/binary>>) -> {'AccruedInterestAmt', Value};
parse_field(<<"160=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'SettlInstMode','DEFAULT'};
	<<"1">> -> {'SettlInstMode','STANDING_INSTRUCTIONS_PROVIDED'};
	<<"2">> -> {'SettlInstMode','SPECIFIC_ALLOCATION_ACCOUNT_OVERRIDING'};
	<<"3">> -> {'SettlInstMode','SPECIFIC_ALLOCATION_ACCOUNT_STANDING'};
	_ -> {'SettlInstMode',Value}
    end;
parse_field(<<"161=", Value/binary>>) -> {'AllocText', Value};
parse_field(<<"162=", Value/binary>>) -> {'SettlInstID', Value};
parse_field(<<"163=", Value/binary>>) ->
    case Value of
	<<"N">> -> {'SettlInstTransType','NEW'};
	<<"C">> -> {'SettlInstTransType','CANCEL'};
	<<"R">> -> {'SettlInstTransType','REPLACE'};
	_ -> {'SettlInstTransType',Value}
    end;
parse_field(<<"164=", Value/binary>>) -> {'EmailThreadID', Value};
parse_field(<<"165=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'SettlInstSource','BROKER'};
	<<"2">> -> {'SettlInstSource','INSTITUTION'};
	_ -> {'SettlInstSource',Value}
    end;
parse_field(<<"166=", Value/binary>>) ->
    case Value of
	<<"CED">> -> {'SettlLocation','CEDEL'};
	<<"DTC">> -> {'SettlLocation','DEPOSITORY_TRUST_COMPANY'};
	<<"EUR">> -> {'SettlLocation','EUROCLEAR'};
	<<"FED">> -> {'SettlLocation','FEDERAL_BOOK_ENTRY'};
	<<"PNY">> -> {'SettlLocation','PHYSICAL'};
	<<"PTC">> -> {'SettlLocation','PARTICIPANT_TRUST_COMPANY'};
	<<"ISO">> -> {'SettlLocation','LOCAL_MARKET_SETTLE_LOCATION'};
	_ -> {'SettlLocation',Value}
    end;
parse_field(<<"167=", Value/binary>>) ->
    case Value of
	<<"BA">> -> {'SecurityType','BANKERS_ACCEPTANCE'};
	<<"CB">> -> {'SecurityType','CONVERTIBLE_BOND'};
	<<"CD">> -> {'SecurityType','CERTIFICATE_OF_DEPOSIT'};
	<<"CMO">> -> {'SecurityType','COLLATERALIZE_MORTGAGE_OBLIGATION'};
	<<"CORP">> -> {'SecurityType','CORPORATE_BOND'};
	<<"CP">> -> {'SecurityType','COMMERCIAL_PAPER'};
	<<"CPP">> -> {'SecurityType','CORPORATE_PRIVATE_PLACEMENT'};
	<<"CS">> -> {'SecurityType','COMMON_STOCK'};
	<<"FHA">> -> {'SecurityType','FEDERAL_HOUSING_AUTHORITY'};
	<<"FHL">> -> {'SecurityType','FEDERAL_HOME_LOAN'};
	<<"FN">> -> {'SecurityType','FEDERAL_NATIONAL_MORTGAGE_ASSOCIATION'};
	<<"FOR">> -> {'SecurityType','FOREIGN_EXCHANGE_CONTRACT'};
	<<"FUT">> -> {'SecurityType','FUTURE'};
	<<"GN">> -> {'SecurityType','GOVERNMENT_NATIONAL_MORTGAGE_ASSOCIATION'};
	<<"GOVT">> -> {'SecurityType','TREASURIES_PLUS_AGENCY_DEBENTURE'};
	<<"MF">> -> {'SecurityType','MUTUAL_FUND'};
	<<"MIO">> -> {'SecurityType','MORTGAGE_INTEREST_ONLY'};
	<<"MPO">> -> {'SecurityType','MORTGAGE_PRINCIPAL_ONLY'};
	<<"MPP">> -> {'SecurityType','MORTGAGE_PRIVATE_PLACEMENT'};
	<<"MPT">> -> {'SecurityType','MISCELLANEOUS_PASSTHRU'};
	<<"MUNI">> -> {'SecurityType','MUNICIPAL_BOND'};
	<<"NONE">> -> {'SecurityType','NO_ISITC_SECURITY_TYPE'};
	<<"OPT">> -> {'SecurityType','OPTION'};
	<<"PS">> -> {'SecurityType','PREFERRED_STOCK'};
	<<"RP">> -> {'SecurityType','REPURCHASE_AGREEMENT'};
	<<"RVRP">> -> {'SecurityType','REVERSE_REPURCHASE_AGREEMENT'};
	<<"SL">> -> {'SecurityType','STUDENT_LOAN_MARKETING_ASSOCIATION'};
	<<"TD">> -> {'SecurityType','TIME_DEPOSIT'};
	<<"USTB">> -> {'SecurityType','US_TREASURY_BILL'};
	<<"WAR">> -> {'SecurityType','WARRANT'};
	<<"ZOO">> -> {'SecurityType','CATS_TIGERS'};
	_ -> {'SecurityType',Value}
    end;
parse_field(<<"168=", Value/binary>>) -> {'EffectiveTime', Value};
parse_field(<<"169=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'StandInstDbType','OTHER'};
	<<"1">> -> {'StandInstDbType','DTC_SID'};
	<<"2">> -> {'StandInstDbType','THOMSON_ALERT'};
	<<"3">> -> {'StandInstDbType','A_GLOBAL_CUSTODIAN'};
	_ -> {'StandInstDbType',Value}
    end;
parse_field(<<"170=", Value/binary>>) -> {'StandInstDbName', Value};
parse_field(<<"171=", Value/binary>>) -> {'StandInstDbID', Value};
parse_field(<<"172=", Value/binary>>) -> {'SettlDeliveryType', binary_to_int(Value)};
parse_field(<<"173=", Value/binary>>) -> {'SettlDepositoryCode', Value};
parse_field(<<"174=", Value/binary>>) -> {'SettlBrkrCode', Value};
parse_field(<<"175=", Value/binary>>) -> {'SettlInstCode', Value};
parse_field(<<"176=", Value/binary>>) -> {'SecuritySettlAgentName', Value};
parse_field(<<"177=", Value/binary>>) -> {'SecuritySettlAgentCode', Value};
parse_field(<<"178=", Value/binary>>) -> {'SecuritySettlAgentAcctNum', Value};
parse_field(<<"179=", Value/binary>>) -> {'SecuritySettlAgentAcctName', Value};
parse_field(<<"180=", Value/binary>>) -> {'SecuritySettlAgentContactName', Value};
parse_field(<<"181=", Value/binary>>) -> {'SecuritySettlAgentContactPhone', Value};
parse_field(<<"182=", Value/binary>>) -> {'CashSettlAgentName', Value};
parse_field(<<"183=", Value/binary>>) -> {'CashSettlAgentCode', Value};
parse_field(<<"184=", Value/binary>>) -> {'CashSettlAgentAcctNum', Value};
parse_field(<<"185=", Value/binary>>) -> {'CashSettlAgentAcctName', Value};
parse_field(<<"186=", Value/binary>>) -> {'CashSettlAgentContactName', Value};
parse_field(<<"187=", Value/binary>>) -> {'CashSettlAgentContactPhone', Value};
parse_field(<<"188=", Value/binary>>) -> {'BidSpotRate', Value};
parse_field(<<"189=", Value/binary>>) -> {'BidForwardPoints', Value};
parse_field(<<"190=", Value/binary>>) -> {'OfferSpotRate', Value};
parse_field(<<"191=", Value/binary>>) -> {'OfferForwardPoints', Value};
parse_field(<<"192=", Value/binary>>) -> {'OrderQty2', Value};
parse_field(<<"193=", Value/binary>>) -> {'FutSettDate2', Value};
parse_field(<<"194=", Value/binary>>) -> {'LastSpotRate', Value};
parse_field(<<"195=", Value/binary>>) -> {'LastForwardPoints', Value};
parse_field(<<"196=", Value/binary>>) -> {'AllocLinkID', Value};
parse_field(<<"197=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'AllocLinkType','FX_NETTING'};
	<<"1">> -> {'AllocLinkType','FX_SWAP'};
	_ -> {'AllocLinkType',Value}
    end;
parse_field(<<"198=", Value/binary>>) -> {'SecondaryOrderID', Value};
parse_field(<<"199=", Value/binary>>) -> {'NoIOIQualifiers', binary_to_int(Value)};
parse_field(<<"200=", Value/binary>>) -> {'MaturityMonthYear', Value};
parse_field(<<"201=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'PutOrCall','PUT'};
	<<"1">> -> {'PutOrCall','CALL'};
	_ -> {'PutOrCall',Value}
    end;
parse_field(<<"202=", Value/binary>>) -> {'StrikePrice', Value};
parse_field(<<"203=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'CoveredOrUncovered','COVERED'};
	<<"1">> -> {'CoveredOrUncovered','UNCOVERED'};
	_ -> {'CoveredOrUncovered',Value}
    end;
parse_field(<<"204=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'CustomerOrFirm','CUSTOMER'};
	<<"1">> -> {'CustomerOrFirm','FIRM'};
	_ -> {'CustomerOrFirm',Value}
    end;
parse_field(<<"205=", Value/binary>>) -> {'MaturityDay', Value};
parse_field(<<"206=", Value/binary>>) -> {'OptAttribute', Value};
parse_field(<<"207=", Value/binary>>) -> {'SecurityExchange', Value};
parse_field(<<"208=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'NotifyBrokerOfCredit','DETAILS_SHOULD_BE_COMMUNICATED'};
	<<"N">> -> {'NotifyBrokerOfCredit','DETAILS_SHOULD_NOT_BE_COMMUNICATED'};
	_ -> {'NotifyBrokerOfCredit',Value}
    end;
parse_field(<<"209=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'AllocHandlInst','MATCH'};
	<<"2">> -> {'AllocHandlInst','FORWARD'};
	<<"3">> -> {'AllocHandlInst','FORWARD_AND_MATCH'};
	_ -> {'AllocHandlInst',Value}
    end;
parse_field(<<"210=", Value/binary>>) -> {'MaxShow', Value};
parse_field(<<"211=", Value/binary>>) -> {'PegDifference', Value};
parse_field(<<"212=", Value/binary>>) -> {'XmlDataLen', binary_to_int(Value)};
parse_field(<<"213=", Value/binary>>) -> {'XmlData', Value};
parse_field(<<"214=", Value/binary>>) -> {'SettlInstRefID', Value};
parse_field(<<"215=", Value/binary>>) -> {'NoRoutingIDs', binary_to_int(Value)};
parse_field(<<"216=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'RoutingType','TARGET_FIRM'};
	<<"2">> -> {'RoutingType','TARGET_LIST'};
	<<"3">> -> {'RoutingType','BLOCK_FIRM'};
	<<"4">> -> {'RoutingType','BLOCK_LIST'};
	_ -> {'RoutingType',Value}
    end;
parse_field(<<"217=", Value/binary>>) -> {'RoutingID', Value};
parse_field(<<"218=", Value/binary>>) -> {'SpreadToBenchmark', Value};
parse_field(<<"219=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'Benchmark','CURVE'};
	<<"2">> -> {'Benchmark','FIVEYR'};
	<<"3">> -> {'Benchmark','OLD5'};
	<<"4">> -> {'Benchmark','TENYR'};
	<<"5">> -> {'Benchmark','OLD10'};
	<<"6">> -> {'Benchmark','THIRTYYR'};
	<<"7">> -> {'Benchmark','OLD30'};
	<<"8">> -> {'Benchmark','THREEMOLIBOR'};
	<<"9">> -> {'Benchmark','SIXMOLIBOR'};
	_ -> {'Benchmark',Value}
    end;
parse_field(<<"223=", Value/binary>>) -> {'CouponRate', Value};
parse_field(<<"231=", Value/binary>>) -> {'ContractMultiplier', Value};
parse_field(<<"262=", Value/binary>>) -> {'MDReqID', Value};
parse_field(<<"263=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'SubscriptionRequestType','SNAPSHOT'};
	<<"1">> -> {'SubscriptionRequestType','SNAPSHOT_PLUS_UPDATES'};
	<<"2">> -> {'SubscriptionRequestType','DISABLE_PREVIOUS'};
	_ -> {'SubscriptionRequestType',Value}
    end;
parse_field(<<"264=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'MarketDepth','FULL_BOOK'};
	<<"1">> -> {'MarketDepth','TOP_OF_BOOK'};
	_ -> {'MarketDepth',Value}
    end;
parse_field(<<"265=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'MDUpdateType','FULL_REFRESH'};
	<<"1">> -> {'MDUpdateType','INCREMENTAL_REFRESH'};
	_ -> {'MDUpdateType',Value}
    end;
parse_field(<<"266=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'AggregatedBook','ONE_BOOK_ENTRY_PER_SIDE_PER_PRICE'};
	<<"N">> -> {'AggregatedBook','MULTIPLE_ENTRIES_PER_SIDE_PER_PRICE_ALLOWED'};
	_ -> {'AggregatedBook',Value}
    end;
parse_field(<<"267=", Value/binary>>) -> {'NoMDEntryTypes', binary_to_int(Value)};
parse_field(<<"268=", Value/binary>>) -> {'NoMDEntries', binary_to_int(Value)};
parse_field(<<"269=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'MDEntryType','BID'};
	<<"1">> -> {'MDEntryType','OFFER'};
	<<"2">> -> {'MDEntryType','TRADE'};
	<<"3">> -> {'MDEntryType','INDEX_VALUE'};
	<<"4">> -> {'MDEntryType','OPENING_PRICE'};
	<<"5">> -> {'MDEntryType','CLOSING_PRICE'};
	<<"6">> -> {'MDEntryType','SETTLEMENT_PRICE'};
	<<"7">> -> {'MDEntryType','TRADING_SESSION_HIGH_PRICE'};
	<<"8">> -> {'MDEntryType','TRADING_SESSION_LOW_PRICE'};
	<<"9">> -> {'MDEntryType','TRADING_SESSION_VWAP_PRICE'};
	_ -> {'MDEntryType',Value}
    end;
parse_field(<<"270=", Value/binary>>) -> {'MDEntryPx', Value};
parse_field(<<"271=", Value/binary>>) -> {'MDEntrySize', Value};
parse_field(<<"272=", Value/binary>>) -> {'MDEntryDate', Value};
parse_field(<<"273=", Value/binary>>) -> {'MDEntryTime', Value};
parse_field(<<"274=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'TickDirection','PLUS_TICK'};
	<<"1">> -> {'TickDirection','ZEROPLUS_TICK'};
	<<"2">> -> {'TickDirection','MINUS_TICK'};
	<<"3">> -> {'TickDirection','ZEROMINUS_TICK'};
	_ -> {'TickDirection',Value}
    end;
parse_field(<<"275=", Value/binary>>) -> {'MDMkt', Value};
parse_field(<<"276=", Value/binary>>) ->
    case Value of
	<<"A">> -> {'QuoteCondition','OPEN_ACTIVE'};
	<<"B">> -> {'QuoteCondition','CLOSED_INACTIVE'};
	<<"C">> -> {'QuoteCondition','EXCHANGE_BEST'};
	<<"D">> -> {'QuoteCondition','CONSOLIDATED_BEST'};
	<<"E">> -> {'QuoteCondition','LOCKED'};
	<<"F">> -> {'QuoteCondition','CROSSED'};
	<<"G">> -> {'QuoteCondition','DEPTH'};
	<<"H">> -> {'QuoteCondition','FAST_TRADING'};
	<<"I">> -> {'QuoteCondition','NONFIRM'};
	_ -> {'QuoteCondition',Value}
    end;
parse_field(<<"277=", Value/binary>>) ->
    case Value of
	<<"A">> -> {'TradeCondition','CASH'};
	<<"B">> -> {'TradeCondition','AVERAGE_PRICE_TRADE'};
	<<"C">> -> {'TradeCondition','CASH_TRADE'};
	<<"D">> -> {'TradeCondition','NEXT_DAY'};
	<<"E">> -> {'TradeCondition','OPENING_REOPENING_TRADE_DETAIL'};
	<<"F">> -> {'TradeCondition','INTRADAY_TRADE_DETAIL'};
	<<"G">> -> {'TradeCondition','RULE_127_TRADE'};
	<<"H">> -> {'TradeCondition','RULE_155_TRADE'};
	<<"I">> -> {'TradeCondition','SOLD_LAST'};
	<<"J">> -> {'TradeCondition','NEXT_DAY_TRADE'};
	<<"K">> -> {'TradeCondition','OPENED'};
	<<"L">> -> {'TradeCondition','SELLER'};
	<<"M">> -> {'TradeCondition','SOLD'};
	<<"N">> -> {'TradeCondition','STOPPED_STOCK'};
	_ -> {'TradeCondition',Value}
    end;
parse_field(<<"278=", Value/binary>>) -> {'MDEntryID', Value};
parse_field(<<"279=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'MDUpdateAction','NEW'};
	<<"1">> -> {'MDUpdateAction','CHANGE'};
	<<"2">> -> {'MDUpdateAction','DELETE'};
	_ -> {'MDUpdateAction',Value}
    end;
parse_field(<<"280=", Value/binary>>) -> {'MDEntryRefID', Value};
parse_field(<<"281=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'MDReqRejReason','UNKNOWN_SYMBOL'};
	<<"1">> -> {'MDReqRejReason','DUPLICATE_MDREQID'};
	<<"2">> -> {'MDReqRejReason','INSUFFICIENT_BANDWIDTH'};
	<<"3">> -> {'MDReqRejReason','INSUFFICIENT_PERMISSIONS'};
	<<"4">> -> {'MDReqRejReason','UNSUPPORTED_SUBSCRIPTIONREQUESTTYPE'};
	<<"5">> -> {'MDReqRejReason','UNSUPPORTED_MARKETDEPTH'};
	<<"6">> -> {'MDReqRejReason','UNSUPPORTED_MDUPDATETYPE'};
	<<"7">> -> {'MDReqRejReason','UNSUPPORTED_AGGREGATEDBOOK'};
	<<"8">> -> {'MDReqRejReason','UNSUPPORTED_MDENTRYTYPE'};
	_ -> {'MDReqRejReason',Value}
    end;
parse_field(<<"282=", Value/binary>>) -> {'MDEntryOriginator', Value};
parse_field(<<"283=", Value/binary>>) -> {'LocationID', Value};
parse_field(<<"284=", Value/binary>>) -> {'DeskID', Value};
parse_field(<<"285=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'DeleteReason','CANCELATION_TRADE_BUST'};
	<<"1">> -> {'DeleteReason','ERROR'};
	_ -> {'DeleteReason',Value}
    end;
parse_field(<<"286=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'OpenCloseSettleFlag','DAILY_OPEN_CLOSE__SETTLEMENT_PRICE'};
	<<"1">> -> {'OpenCloseSettleFlag','SESSION_OPEN_CLOSE__SETTLEMENT_PRICE'};
	<<"2">> -> {'OpenCloseSettleFlag','DELIVERY_SETTLEMENT_PRICE'};
	_ -> {'OpenCloseSettleFlag',Value}
    end;
parse_field(<<"287=", Value/binary>>) -> {'SellerDays', binary_to_int(Value)};
parse_field(<<"288=", Value/binary>>) -> {'MDEntryBuyer', Value};
parse_field(<<"289=", Value/binary>>) -> {'MDEntrySeller', Value};
parse_field(<<"290=", Value/binary>>) -> {'MDEntryPositionNo', binary_to_int(Value)};
parse_field(<<"291=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'FinancialStatus','BANKRUPT'};
	_ -> {'FinancialStatus',Value}
    end;
parse_field(<<"292=", Value/binary>>) ->
    case Value of
	<<"A">> -> {'CorporateAction','EXDIVIDEND'};
	<<"B">> -> {'CorporateAction','EXDISTRIBUTION'};
	<<"C">> -> {'CorporateAction','EXRIGHTS'};
	<<"D">> -> {'CorporateAction','NEW'};
	<<"E">> -> {'CorporateAction','EXINTEREST'};
	_ -> {'CorporateAction',Value}
    end;
parse_field(<<"293=", Value/binary>>) -> {'DefBidSize', Value};
parse_field(<<"294=", Value/binary>>) -> {'DefOfferSize', Value};
parse_field(<<"295=", Value/binary>>) -> {'NoQuoteEntries', binary_to_int(Value)};
parse_field(<<"296=", Value/binary>>) -> {'NoQuoteSets', binary_to_int(Value)};
parse_field(<<"297=", Value/binary>>) -> {'QuoteAckStatus', binary_to_int(Value)};
parse_field(<<"298=", Value/binary>>) -> {'QuoteCancelType', binary_to_int(Value)};
parse_field(<<"299=", Value/binary>>) -> {'QuoteEntryID', Value};
parse_field(<<"300=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'QuoteRejectReason','UNKNOWN_SYMBOL'};
	<<"2">> -> {'QuoteRejectReason','EXCHANGE'};
	<<"3">> -> {'QuoteRejectReason','QUOTE_REQUEST_EXCEEDS_LIMIT'};
	<<"4">> -> {'QuoteRejectReason','TOO_LATE_TO_ENTER'};
	<<"5">> -> {'QuoteRejectReason','UNKNOWN_QUOTE'};
	<<"6">> -> {'QuoteRejectReason','DUPLICATE_QUOTE_7'};
	<<"8">> -> {'QuoteRejectReason','INVALID_PRICE'};
	<<"9">> -> {'QuoteRejectReason','NOT_AUTHORIZED_TO_QUOTE_SECURITY'};
	_ -> {'QuoteRejectReason',Value}
    end;
parse_field(<<"301=", Value/binary>>) -> {'QuoteResponseLevel', binary_to_int(Value)};
parse_field(<<"302=", Value/binary>>) -> {'QuoteSetID', Value};
parse_field(<<"303=", Value/binary>>) -> {'QuoteRequestType', binary_to_int(Value)};
parse_field(<<"304=", Value/binary>>) -> {'TotQuoteEntries', binary_to_int(Value)};
parse_field(<<"305=", Value/binary>>) -> {'UnderlyingIDSource', Value};
parse_field(<<"306=", Value/binary>>) -> {'UnderlyingIssuer', Value};
parse_field(<<"307=", Value/binary>>) -> {'UnderlyingSecurityDesc', Value};
parse_field(<<"308=", Value/binary>>) -> {'UnderlyingSecurityExchange', Value};
parse_field(<<"309=", Value/binary>>) -> {'UnderlyingSecurityID', Value};
parse_field(<<"310=", Value/binary>>) -> {'UnderlyingSecurityType', Value};
parse_field(<<"311=", Value/binary>>) -> {'UnderlyingSymbol', Value};
parse_field(<<"312=", Value/binary>>) -> {'UnderlyingSymbolSfx', Value};
parse_field(<<"313=", Value/binary>>) -> {'UnderlyingMaturityMonthYear', Value};
parse_field(<<"314=", Value/binary>>) -> {'UnderlyingMaturityDay', Value};
parse_field(<<"315=", Value/binary>>) -> {'UnderlyingPutOrCall', binary_to_int(Value)};
parse_field(<<"316=", Value/binary>>) -> {'UnderlyingStrikePrice', Value};
parse_field(<<"317=", Value/binary>>) -> {'UnderlyingOptAttribute', Value};
parse_field(<<"318=", Value/binary>>) -> {'UnderlyingCurrency', Value};
parse_field(<<"319=", Value/binary>>) -> {'RatioQty', Value};
parse_field(<<"320=", Value/binary>>) -> {'SecurityReqID', Value};
parse_field(<<"321=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'SecurityRequestType','REQUEST_SECURITY_IDENTITY_AND_SPECIFICATIONS'};
	<<"1">> -> {'SecurityRequestType','REQUEST_SECURITY_IDENTITY_FOR_THE_SPECIFICATIONS_PROVIDED'};
	<<"2">> -> {'SecurityRequestType','REQUEST_LIST_SECURITY_TYPES'};
	<<"3">> -> {'SecurityRequestType','REQUEST_LIST_SECURITIES'};
	_ -> {'SecurityRequestType',Value}
    end;
parse_field(<<"322=", Value/binary>>) -> {'SecurityResponseID', Value};
parse_field(<<"323=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'SecurityResponseType','ACCEPT_SECURITY_PROPOSAL_AS_IS'};
	<<"2">> -> {'SecurityResponseType','ACCEPT_SECURITY_PROPOSAL_WITH_REVISIONS_AS_INDICATED_IN_THE_MESSAGE'};
	<<"3">> -> {'SecurityResponseType','LIST_OF_SECURITY_TYPES_RETURNED_PER_REQUEST'};
	<<"4">> -> {'SecurityResponseType','LIST_OF_SECURITIES_RETURNED_PER_REQUEST'};
	<<"5">> -> {'SecurityResponseType','REJECT_SECURITY_PROPOSAL'};
	<<"6">> -> {'SecurityResponseType','CAN_NOT_MATCH_SELECTION_CRITERIA'};
	_ -> {'SecurityResponseType',Value}
    end;
parse_field(<<"324=", Value/binary>>) -> {'SecurityStatusReqID', Value};
parse_field(<<"325=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'UnsolicitedIndicator','MESSAGE_IS_BEING_SENT_UNSOLICITED'};
	<<"N">> -> {'UnsolicitedIndicator','MESSAGE_IS_BEING_SENT_AS_A_RESULT_OF_A_PRIOR_REQUEST'};
	_ -> {'UnsolicitedIndicator',Value}
    end;
parse_field(<<"326=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'SecurityTradingStatus','OPENING_DELAY'};
	<<"2">> -> {'SecurityTradingStatus','TRADING_HALT'};
	<<"3">> -> {'SecurityTradingStatus','RESUME'};
	<<"4">> -> {'SecurityTradingStatus','NO_OPENNO_RESUME'};
	<<"5">> -> {'SecurityTradingStatus','PRICE_INDICATION'};
	<<"6">> -> {'SecurityTradingStatus','TRADING_RANGE_INDICATION'};
	<<"7">> -> {'SecurityTradingStatus','MARKET_IMBALANCE_BUY'};
	<<"8">> -> {'SecurityTradingStatus','MARKET_IMBALANCE_SELL'};
	<<"9">> -> {'SecurityTradingStatus','MARKET_ON_CLOSE_IMBALANCE_BUY'};
	<<"10">> -> {'SecurityTradingStatus','MARKET_ON_CLOSE_IMBALANCE_SELL'};
	<<"11">> -> {'SecurityTradingStatus','NOT_ASSIGNED'};
	<<"12">> -> {'SecurityTradingStatus','NO_MARKET_IMBALANCE'};
	<<"13">> -> {'SecurityTradingStatus','NO_MARKET_ON_CLOSE_IMBALANCE'};
	<<"14">> -> {'SecurityTradingStatus','ITS_PREOPENING'};
	<<"15">> -> {'SecurityTradingStatus','NEW_PRICE_INDICATION'};
	<<"16">> -> {'SecurityTradingStatus','TRADE_DISSEMINATION_TIME'};
	<<"17">> -> {'SecurityTradingStatus','READY_TO_TRADE'};
	<<"18">> -> {'SecurityTradingStatus','NOT_AVAILABLE_FOR_TRADING'};
	<<"19">> -> {'SecurityTradingStatus','NOT_TRADED_ON_THIS_MARKET'};
	<<"20">> -> {'SecurityTradingStatus','UNKNOWN_OR_INVALID'};
	_ -> {'SecurityTradingStatus',Value}
    end;
parse_field(<<"327=", Value/binary>>) ->
    case Value of
	<<"I">> -> {'HaltReason','ORDER_IMBALANCE'};
	<<"X">> -> {'HaltReason','EQUIPMENT_CHANGEOVER'};
	<<"P">> -> {'HaltReason','NEWS_PENDING'};
	<<"D">> -> {'HaltReason','NEWS_DISSEMINATION'};
	<<"E">> -> {'HaltReason','ORDER_INFLUX'};
	<<"M">> -> {'HaltReason','ADDITIONAL_INFORMATION'};
	_ -> {'HaltReason',Value}
    end;
parse_field(<<"328=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'InViewOfCommon','HALT_WAS_DUE_TO_COMMON_STOCK_BEING_HALTED'};
	<<"N">> -> {'InViewOfCommon','HALT_WAS_NOT_RELATED_TO_A_HALT_OF_THE_COMMON_STOCK'};
	_ -> {'InViewOfCommon',Value}
    end;
parse_field(<<"329=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'DueToRelated','HALT_WAS_DUE_TO_RELATED_SECURITY_BEING_HALTED'};
	<<"N">> -> {'DueToRelated','HALT_WAS_NOT_RELATED_TO_A_HALT_OF_THE_RELATED_SECURITY'};
	_ -> {'DueToRelated',Value}
    end;
parse_field(<<"330=", Value/binary>>) -> {'BuyVolume', Value};
parse_field(<<"331=", Value/binary>>) -> {'SellVolume', Value};
parse_field(<<"332=", Value/binary>>) -> {'HighPx', Value};
parse_field(<<"333=", Value/binary>>) -> {'LowPx', Value};
parse_field(<<"334=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'Adjustment','CANCEL'};
	<<"2">> -> {'Adjustment','ERROR'};
	<<"3">> -> {'Adjustment','CORRECTION'};
	_ -> {'Adjustment',Value}
    end;
parse_field(<<"335=", Value/binary>>) -> {'TradSesReqID', Value};
parse_field(<<"336=", Value/binary>>) -> {'TradingSessionID', Value};
parse_field(<<"337=", Value/binary>>) -> {'ContraTrader', Value};
parse_field(<<"338=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'TradSesMethod','ELECTRONIC'};
	<<"2">> -> {'TradSesMethod','OPEN_OUTCRY'};
	<<"3">> -> {'TradSesMethod','TWO_PARTY'};
	_ -> {'TradSesMethod',Value}
    end;
parse_field(<<"339=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'TradSesMode','TESTING'};
	<<"2">> -> {'TradSesMode','SIMULATED'};
	<<"3">> -> {'TradSesMode','PRODUCTION'};
	_ -> {'TradSesMode',Value}
    end;
parse_field(<<"340=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'TradSesStatus','HALTED'};
	<<"2">> -> {'TradSesStatus','OPEN'};
	<<"3">> -> {'TradSesStatus','CLOSED'};
	<<"4">> -> {'TradSesStatus','PREOPEN'};
	<<"5">> -> {'TradSesStatus','PRECLOSE'};
	_ -> {'TradSesStatus',Value}
    end;
parse_field(<<"341=", Value/binary>>) -> {'TradSesStartTime', Value};
parse_field(<<"342=", Value/binary>>) -> {'TradSesOpenTime', Value};
parse_field(<<"343=", Value/binary>>) -> {'TradSesPreCloseTime', Value};
parse_field(<<"344=", Value/binary>>) -> {'TradSesCloseTime', Value};
parse_field(<<"345=", Value/binary>>) -> {'TradSesEndTime', Value};
parse_field(<<"346=", Value/binary>>) -> {'NumberOfOrders', binary_to_int(Value)};
parse_field(<<"347=", Value/binary>>) -> {'MessageEncoding', Value};
parse_field(<<"348=", Value/binary>>) -> {'EncodedIssuerLen', binary_to_int(Value)};
parse_field(<<"349=", Value/binary>>) -> {'EncodedIssuer', Value};
parse_field(<<"350=", Value/binary>>) -> {'EncodedSecurityDescLen', binary_to_int(Value)};
parse_field(<<"351=", Value/binary>>) -> {'EncodedSecurityDesc', Value};
parse_field(<<"352=", Value/binary>>) -> {'EncodedListExecInstLen', binary_to_int(Value)};
parse_field(<<"353=", Value/binary>>) -> {'EncodedListExecInst', Value};
parse_field(<<"354=", Value/binary>>) -> {'EncodedTextLen', binary_to_int(Value)};
parse_field(<<"355=", Value/binary>>) -> {'EncodedText', Value};
parse_field(<<"356=", Value/binary>>) -> {'EncodedSubjectLen', binary_to_int(Value)};
parse_field(<<"357=", Value/binary>>) -> {'EncodedSubject', Value};
parse_field(<<"358=", Value/binary>>) -> {'EncodedHeadlineLen', binary_to_int(Value)};
parse_field(<<"359=", Value/binary>>) -> {'EncodedHeadline', Value};
parse_field(<<"360=", Value/binary>>) -> {'EncodedAllocTextLen', binary_to_int(Value)};
parse_field(<<"361=", Value/binary>>) -> {'EncodedAllocText', Value};
parse_field(<<"362=", Value/binary>>) -> {'EncodedUnderlyingIssuerLen', binary_to_int(Value)};
parse_field(<<"363=", Value/binary>>) -> {'EncodedUnderlyingIssuer', Value};
parse_field(<<"364=", Value/binary>>) -> {'EncodedUnderlyingSecurityDescLen', binary_to_int(Value)};
parse_field(<<"365=", Value/binary>>) -> {'EncodedUnderlyingSecurityDesc', Value};
parse_field(<<"366=", Value/binary>>) -> {'AllocPrice', Value};
parse_field(<<"367=", Value/binary>>) -> {'QuoteSetValidUntilTime', Value};
parse_field(<<"368=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'QuoteEntryRejectReason','UNKNOWN_SYMBOL'};
	<<"2">> -> {'QuoteEntryRejectReason','EXCHANGE'};
	<<"3">> -> {'QuoteEntryRejectReason','QUOTE_EXCEEDS_LIMIT'};
	<<"4">> -> {'QuoteEntryRejectReason','TOO_LATE_TO_ENTER'};
	<<"5">> -> {'QuoteEntryRejectReason','UNKNOWN_QUOTE'};
	<<"6">> -> {'QuoteEntryRejectReason','DUPLICATE_QUOTE'};
	<<"7">> -> {'QuoteEntryRejectReason','INVALID_BIDASK_SPREAD'};
	<<"8">> -> {'QuoteEntryRejectReason','INVALID_PRICE'};
	<<"9">> -> {'QuoteEntryRejectReason','NOT_AUTHORIZED_TO_QUOTE_SECURITY'};
	_ -> {'QuoteEntryRejectReason',Value}
    end;
parse_field(<<"369=", Value/binary>>) -> {'LastMsgSeqNumProcessed', binary_to_int(Value)};
parse_field(<<"370=", Value/binary>>) -> {'OnBehalfOfSendingTime', Value};
parse_field(<<"371=", Value/binary>>) -> {'RefTagID', binary_to_int(Value)};
parse_field(<<"372=", Value/binary>>) -> {'RefMsgType', Value};
parse_field(<<"373=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'SessionRejectReason','INVALID_TAG_NUMBER'};
	<<"1">> -> {'SessionRejectReason','REQUIRED_TAG_MISSING'};
	<<"2">> -> {'SessionRejectReason','TAG_NOT_DEFINED_FOR_THIS_MESSAGE_TYPE'};
	<<"3">> -> {'SessionRejectReason','UNDEFINED_TAG'};
	<<"4">> -> {'SessionRejectReason','TAG_SPECIFIED_WITHOUT_A_VALUE'};
	<<"5">> -> {'SessionRejectReason','VALUE_IS_INCORRECT'};
	<<"6">> -> {'SessionRejectReason','INCORRECT_DATA_FORMAT_FOR_VALUE'};
	<<"7">> -> {'SessionRejectReason','DECRYPTION_PROBLEM'};
	<<"8">> -> {'SessionRejectReason','SIGNATURE_PROBLEM'};
	<<"9">> -> {'SessionRejectReason','COMPID_PROBLEM'};
	<<"10">> -> {'SessionRejectReason','SENDINGTIME_ACCURACY_PROBLEM'};
	<<"11">> -> {'SessionRejectReason','INVALID_MESSAGE_TYPE'};
	_ -> {'SessionRejectReason',Value}
    end;
parse_field(<<"374=", Value/binary>>) ->
    case Value of
	<<"N">> -> {'BidRequestTransType','NEW'};
	<<"C">> -> {'BidRequestTransType','CANCEL'};
	_ -> {'BidRequestTransType',Value}
    end;
parse_field(<<"375=", Value/binary>>) -> {'ContraBroker', Value};
parse_field(<<"376=", Value/binary>>) -> {'ComplianceID', Value};
parse_field(<<"377=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'SolicitedFlag','WAS_SOLCITIED'};
	<<"N">> -> {'SolicitedFlag','WAS_NOT_SOLICITED'};
	_ -> {'SolicitedFlag',Value}
    end;
parse_field(<<"378=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'ExecRestatementReason','GT_CORPORATE_ACTION'};
	<<"1">> -> {'ExecRestatementReason','GT_RENEWAL_RESTATEMENT'};
	<<"2">> -> {'ExecRestatementReason','VERBAL_CHANGE'};
	<<"3">> -> {'ExecRestatementReason','REPRICING_OF_ORDER'};
	<<"4">> -> {'ExecRestatementReason','BROKER_OPTION'};
	<<"5">> -> {'ExecRestatementReason','PARTIAL_DECLINE_OF_ORDERQTY'};
	_ -> {'ExecRestatementReason',Value}
    end;
parse_field(<<"379=", Value/binary>>) -> {'BusinessRejectRefID', Value};
parse_field(<<"380=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'BusinessRejectReason','OTHER'};
	<<"1">> -> {'BusinessRejectReason','UNKOWN_ID'};
	<<"2">> -> {'BusinessRejectReason','UNKNOWN_SECURITY'};
	<<"3">> -> {'BusinessRejectReason','UNSUPPORTED_MESSAGE_TYPE'};
	<<"4">> -> {'BusinessRejectReason','APPLICATION_NOT_AVAILABLE'};
	<<"5">> -> {'BusinessRejectReason','CONDITIONALLY_REQUIRED_FIELD_MISSING'};
	_ -> {'BusinessRejectReason',Value}
    end;
parse_field(<<"381=", Value/binary>>) -> {'GrossTradeAmt', Value};
parse_field(<<"382=", Value/binary>>) -> {'NoContraBrokers', binary_to_int(Value)};
parse_field(<<"383=", Value/binary>>) -> {'MaxMessageSize', binary_to_int(Value)};
parse_field(<<"384=", Value/binary>>) -> {'NoMsgTypes', binary_to_int(Value)};
parse_field(<<"385=", Value/binary>>) ->
    case Value of
	<<"S">> -> {'MsgDirection','SEND'};
	<<"R">> -> {'MsgDirection','RECEIVE'};
	_ -> {'MsgDirection',Value}
    end;
parse_field(<<"386=", Value/binary>>) -> {'NoTradingSessions', binary_to_int(Value)};
parse_field(<<"387=", Value/binary>>) -> {'TotalVolumeTraded', Value};
parse_field(<<"388=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'DiscretionInst','RELATED_TO_DISPLAYED_PRICE'};
	<<"1">> -> {'DiscretionInst','RELATED_TO_MARKET_PRICE'};
	<<"2">> -> {'DiscretionInst','RELATED_TO_PRIMARY_PRICE'};
	<<"3">> -> {'DiscretionInst','RELATED_TO_LOCAL_PRIMARY_PRICE'};
	<<"4">> -> {'DiscretionInst','RELATED_TO_MIDPOINT_PRICE'};
	<<"5">> -> {'DiscretionInst','RELATED_TO_LAST_TRADE_PRICE'};
	_ -> {'DiscretionInst',Value}
    end;
parse_field(<<"389=", Value/binary>>) -> {'DiscretionOffset', Value};
parse_field(<<"390=", Value/binary>>) -> {'BidID', Value};
parse_field(<<"391=", Value/binary>>) -> {'ClientBidID', Value};
parse_field(<<"392=", Value/binary>>) -> {'ListName', Value};
parse_field(<<"393=", Value/binary>>) -> {'TotalNumSecurities', binary_to_int(Value)};
parse_field(<<"394=", Value/binary>>) -> {'BidType', binary_to_int(Value)};
parse_field(<<"395=", Value/binary>>) -> {'NumTickets', binary_to_int(Value)};
parse_field(<<"396=", Value/binary>>) -> {'SideValue1', Value};
parse_field(<<"397=", Value/binary>>) -> {'SideValue2', Value};
parse_field(<<"398=", Value/binary>>) -> {'NoBidDescriptors', binary_to_int(Value)};
parse_field(<<"399=", Value/binary>>) -> {'BidDescriptorType', binary_to_int(Value)};
parse_field(<<"400=", Value/binary>>) -> {'BidDescriptor', Value};
parse_field(<<"401=", Value/binary>>) -> {'SideValueInd', binary_to_int(Value)};
parse_field(<<"402=", Value/binary>>) -> {'LiquidityPctLow', Value};
parse_field(<<"403=", Value/binary>>) -> {'LiquidityPctHigh', Value};
parse_field(<<"404=", Value/binary>>) -> {'LiquidityValue', Value};
parse_field(<<"405=", Value/binary>>) -> {'EFPTrackingError', Value};
parse_field(<<"406=", Value/binary>>) -> {'FairValue', Value};
parse_field(<<"407=", Value/binary>>) -> {'OutsideIndexPct', Value};
parse_field(<<"408=", Value/binary>>) -> {'ValueOfFutures', Value};
parse_field(<<"409=", Value/binary>>) -> {'LiquidityIndType', binary_to_int(Value)};
parse_field(<<"410=", Value/binary>>) -> {'WtAverageLiquidity', Value};
parse_field(<<"411=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'ExchangeForPhysical','TRUE'};
	<<"N">> -> {'ExchangeForPhysical','FALSE'};
	_ -> {'ExchangeForPhysical',Value}
    end;
parse_field(<<"412=", Value/binary>>) -> {'OutMainCntryUIndex', Value};
parse_field(<<"413=", Value/binary>>) -> {'CrossPercent', Value};
parse_field(<<"414=", Value/binary>>) -> {'ProgRptReqs', binary_to_int(Value)};
parse_field(<<"415=", Value/binary>>) -> {'ProgPeriodInterval', binary_to_int(Value)};
parse_field(<<"416=", Value/binary>>) -> {'IncTaxInd', binary_to_int(Value)};
parse_field(<<"417=", Value/binary>>) -> {'NumBidders', binary_to_int(Value)};
parse_field(<<"418=", Value/binary>>) -> {'TradeType', Value};
parse_field(<<"419=", Value/binary>>) -> {'BasisPxType', Value};
parse_field(<<"420=", Value/binary>>) -> {'NoBidComponents', binary_to_int(Value)};
parse_field(<<"421=", Value/binary>>) -> {'Country', Value};
parse_field(<<"422=", Value/binary>>) -> {'TotNoStrikes', binary_to_int(Value)};
parse_field(<<"423=", Value/binary>>) -> {'PriceType', binary_to_int(Value)};
parse_field(<<"424=", Value/binary>>) -> {'DayOrderQty', Value};
parse_field(<<"425=", Value/binary>>) -> {'DayCumQty', Value};
parse_field(<<"426=", Value/binary>>) -> {'DayAvgPx', Value};
parse_field(<<"427=", Value/binary>>) ->
    case Value of
	<<"0">> -> {'GTBookingInst','BOOK_OUT_ALL_TRADES_ON_DAY_OF_EXECUTION'};
	<<"1">> -> {'GTBookingInst','ACCUMULATE_EXECUTIONS_UNTIL_ORDER_IS_FILLED_OR_EXPIRES'};
	<<"2">> -> {'GTBookingInst','ACCUMULATE_UNTIL_VERBALLY_NOTIFIED_OTHERWISE'};
	_ -> {'GTBookingInst',Value}
    end;
parse_field(<<"428=", Value/binary>>) -> {'NoStrikes', binary_to_int(Value)};
parse_field(<<"429=", Value/binary>>) -> {'ListStatusType', binary_to_int(Value)};
parse_field(<<"430=", Value/binary>>) -> {'NetGrossInd', binary_to_int(Value)};
parse_field(<<"431=", Value/binary>>) -> {'ListOrderStatus', binary_to_int(Value)};
parse_field(<<"432=", Value/binary>>) -> {'ExpireDate', Value};
parse_field(<<"433=", Value/binary>>) -> {'ListExecInstType', Value};
parse_field(<<"434=", Value/binary>>) -> {'CxlRejResponseTo', Value};
parse_field(<<"435=", Value/binary>>) -> {'UnderlyingCouponRate', Value};
parse_field(<<"436=", Value/binary>>) -> {'UnderlyingContractMultiplier', Value};
parse_field(<<"437=", Value/binary>>) -> {'ContraTradeQty', Value};
parse_field(<<"438=", Value/binary>>) -> {'ContraTradeTime', Value};
parse_field(<<"439=", Value/binary>>) -> {'ClearingFirm', Value};
parse_field(<<"440=", Value/binary>>) -> {'ClearingAccount', Value};
parse_field(<<"441=", Value/binary>>) -> {'LiquidityNumSecurities', binary_to_int(Value)};
parse_field(<<"442=", Value/binary>>) -> {'MultiLegReportingType', Value};
parse_field(<<"443=", Value/binary>>) -> {'StrikeTime', Value};
parse_field(<<"444=", Value/binary>>) -> {'ListStatusText', Value};
parse_field(<<"445=", Value/binary>>) -> {'EncodedListStatusTextLen', binary_to_int(Value)};
parse_field(<<"446=", Value/binary>>) -> {'EncodedListStatusText', Value};
parse_field(<<"483=", Value/binary>>) -> {'TransBkdTime', Value};
parse_field(<<"528=", Value/binary>>) ->
    case Value of
	<<"P">> -> {'OrderCapacity','PRINCIPAL'};
	<<"A">> -> {'OrderCapacity','AGENT'};
	_ -> {'OrderCapacity',Value}
    end;
parse_field(<<"529=", Value/binary>>) ->
    case Value of
	<<"B">> -> {'OrderRestrictions','ISSUER_HOLDING'};
	<<"C">> -> {'OrderRestrictions','ISSUE_PRICE_STABILIZATION'};
	<<"5">> -> {'OrderRestrictions','ACTING_AS_MARKET_MAKER'};
	_ -> {'OrderRestrictions',Value}
    end;
parse_field(<<"571=", Value/binary>>) -> {'TradeReportID', Value};
parse_field(<<"572=", Value/binary>>) -> {'TradeReportRefId', Value};
parse_field(<<"577=", Value/binary>>) -> {'ClearingInstruction', binary_to_int(Value)};
parse_field(<<"625=", Value/binary>>) -> {'TradingsessionSubId', Value};
parse_field(<<"700=", Value/binary>>) -> {'ReversalIndicator', Value};
parse_field(<<"751=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'TradeReportRejectReason','INVALID_PARTY_INFORMATION'};
	<<"2">> -> {'TradeReportRejectReason','UNKNOWN_INSTRUMENT'};
	<<"3">> -> {'TradeReportRejectReason','UNAUTHORIZED_TO_REPORT_TRADES'};
	<<"4">> -> {'TradeReportRejectReason','INVALID_TRADE_TYPE'};
	<<"6">> -> {'TradeReportRejectReason','INCORRECT_DATA_FORMAT_FOR_VALUE'};
	<<"99">> -> {'TradeReportRejectReason','OTHER'};
	_ -> {'TradeReportRejectReason',Value}
    end;
parse_field(<<"820=", Value/binary>>) -> {'TradeLinkId', Value};
parse_field(<<"828=", Value/binary>>) -> {'TrdType', binary_to_int(Value)};
parse_field(<<"852=", Value/binary>>) -> {'PublishTrdIndicator', Value};
parse_field(<<"856=", Value/binary>>) -> {'TradeReportType', binary_to_int(Value)};
parse_field(<<"880=", Value/binary>>) -> {'TrdMatchId', Value};
parse_field(<<"881=", Value/binary>>) -> {'SecondaryTradeReportRefID', Value};
parse_field(<<"939=", Value/binary>>) -> {'TrdRptStatus', binary_to_int(Value)};
parse_field(<<"5149=", Value/binary>>) -> {'Memo', Value};
parse_field(<<"5815=", Value/binary>>) -> {'SubMktID', Value};
parse_field(<<"5817=", Value/binary>>) -> {'ContraOrderRestrictions', Value};
parse_field(<<"6169=", Value/binary>>) -> {'DisseminationTime', Value};
parse_field(<<"6204=", Value/binary>>) -> {'TimestampOwn', Value};
parse_field(<<"6205=", Value/binary>>) -> {'TimestampCounterpart', Value};
parse_field(<<"6206=", Value/binary>>) ->
    case Value of
	<<"I">> -> {'InternalExternal','INTERNAL'};
	<<"E">> -> {'InternalExternal','EXTERNAL'};
	_ -> {'InternalExternal',Value}
    end;
parse_field(<<"6209=", Value/binary>>) -> {'ClRefID', Value};
parse_field(<<"9140=", Value/binary>>) ->
    case Value of
	<<"Y">> -> {'DisplayInst','DISPLAY'};
	<<"N">> -> {'DisplayInst','NON_DISPLAY'};
	<<"D">> -> {'DisplayInst','DISPLAY_OVERRIDE_POST_TRADE_ANONYMITY'};
	<<"R">> -> {'DisplayInst','NON_DISPLAY_OVERRIDE_POST_TRADE_ANONYMITY'};
	<<"I">> -> {'DisplayInst','IMBALANCE_ONLY'};
	_ -> {'DisplayInst',Value}
    end;
parse_field(<<"9165=", Value/binary>>) -> {'RFQReferenceNo', Value};
parse_field(<<"9292=", Value/binary>>) -> {'MICCode', Value};
parse_field(<<"9355=", Value/binary>>) ->
    case Value of
	<<"C">> -> {'CrossTradeFlag','CLOSING_CROSS'};
	<<"O">> -> {'CrossTradeFlag','OPENING_CROSS'};
	<<"I">> -> {'CrossTradeFlag','INTRADAY_CROSS'};
	_ -> {'CrossTradeFlag',Value}
    end;
parse_field(<<"9822=", Value/binary>>) -> {'ClearingPrice', Value};
parse_field(<<"9854=", Value/binary>>) -> {'OverrideFlag', Value};
parse_field(<<"9847=", Value/binary>>) ->
    case Value of
	<<"B">> -> {'LockedInStatus','BROKEN'};
	<<"M">> -> {'LockedInStatus','MATCHED'};
	_ -> {'LockedInStatus',Value}
    end;
parse_field(<<"9855=", Value/binary>>) ->
    case Value of
	<<"1">> -> {'DelayedDissemination','60_MINUTES'};
	<<"2">> -> {'DelayedDissemination','180_MINUTES'};
	<<"3">> -> {'DelayedDissemination','12:00_TOMORROW'};
	<<"4">> -> {'DelayedDissemination','UNTIL_END_OF_TRADING_DAY'};
	<<"5">> -> {'DelayedDissemination','UNTIL_NEXT_TRADING_DAY'};
	<<"6">> -> {'DelayedDissemination','UNTIL_END_OF_SECOND_TRADING_DAY'};
	<<"7">> -> {'DelayedDissemination','UNTIL_END_OF_THIRD_TRADING_DAY'};
	<<"8">> -> {'DelayedDissemination','UNTIL_END_OF_CURRENT_DAY'};
	_ -> {'DelayedDissemination',Value}
    end;
parse_field(<<"9856=", Value/binary>>) ->
    case Value of
	<<"B">> -> {'BreakIndicator','BUYER_SUBMITTED_BREAK_REQUEST'};
	<<"S">> -> {'BreakIndicator','SELLER_SUBMITTED_BREAK_REQUEST'};
	<<"X">> -> {'BreakIndicator','TRADE_BROKEN_BY_BOTH_BUYER_AND_SELLER'};
	<<"L">> -> {'BreakIndicator','TRADE_BROKEN_THROUGH_MARKET_CENTER'};
	_ -> {'BreakIndicator',Value}
    end;
parse_field(<<"9857=", Value/binary>>) ->
    case Value of
	<<"M">> -> {'LockedIn','MATCHED'};
	_ -> {'LockedIn',Value}
    end;
parse_field(<<"9861=", Value/binary>>) -> {'BrSeqNbr', Value};
parse_field(<<"9862=", Value/binary>>) -> {'ContraTradePA', Value};
parse_field(<<"9863=", Value/binary>>) -> {'ContraClearingAcct', Value};
parse_field(<<"9882=", Value/binary>>) -> {'LiquidityFlag', Value};
parse_field(<<"1003=", Value/binary>>) -> {'TradeId', Value};
parse_field(<<>>) -> unknown.

binary_to_int(X) -> list_to_integer(binary_to_list(X)).
