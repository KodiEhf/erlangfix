-module(fix_parser).
-behaviour(gen_parser).
-include("fix.hrl").
-export([parse/1]).
-compile(export_all).

%% Data = <<"8=FIX.4.2|9=178|35=8|49=PHLX|56=PERS|52=20071123-05:30:00.000|11=ATOMNOCCC9990900|20=3|150=E|39=E|55=MSFT|167=CS|54=1|38=15|40=2|44=15|58=PHLX EQUITY TESTING|59=0|47=C|32=0|31=0|151=15|14=0|6=0|10=128|">>.

%%<<"8=FIX.4.2|9=65|35=A|49=SERVER|56=CLIENT|34=177|52=20090107-18:15:16|98=0|108=30|10=062|">>

%% We need a mapping of integers to tag-names
%% We know that a message ends with "10=xyz |"
%% Parse should return a list of "parsed messages" and a binary rest
%% BodyLength is always the second field in the message,
%% length refers to the message length up to checksum field

get_data() ->
    Data = <<"8=FIX.4.2|9=178|35=8|49=PHLX|56=PERS|52=20071123-05:30:00.000|11=ATOMNOCCC9990900|20=3|150=E|39=E|55=MSFT|167=CS|54=1|38=15|40=2|44=15|58=PHLX EQUITY TESTING|59=0|47=C|32=0|31=0|151=15|14=0|6=0|10=128|">>,
    binary:replace(Data, <<"|">>, <<2#01>>,[global]).

p(<<_Data/binary>>, 0) ->
    ok;
p(<<Data/binary>>, Count) ->
    parse(Data),
    p(Data, Count-1).

parse(<<Data/binary>>) ->
    %% if something goes wrong here - the message is incomplete
    
    %% Retreive the first field from the message, which is 'BeginString'
    [First, Rest] = binary:split(Data, [<<?SOH>>], []),
    {'BeginString', _Value} = field_parse(First),
    %% Retreive the second field from the message, which is 'BodyLength'
    [Second, _Message] = binary:split(Rest, [<<?SOH>>], []),
    {'BodyLength', BodyLength} = field_parse(Second),
    %% Calculate the total length of the message, minus the 'CheckSum' field
    MessageLength = BodyLength + length(binary_to_list(First)) + length(binary_to_list(Second)) + 2, %%2 x SOH
    TotalMessageLength = MessageLength + 6,
						%io:format("TotalMessageLength = ~p~n", [TotalMessageLength]),
    %% The rest of the message we have to parse
    %%MessageBody = binary:part(Message, 0, Length),
    %% Validate the checksum
    %%{'CheckSum', Sum} = field_parse(binary:part(Message, Length, 6)), %%the length of the checksum field is always 6
						%io:format("Sum = ~p~n", [Sum]),
    <<CheckSumData:MessageLength/binary, _/binary>> = <<Data/binary>>,
    CheckSum = check_sum(CheckSumData),
    %% Find the message Type
    <<Current:TotalMessageLength/binary, Rest/binary>> = <<Data/binary>>,
    FieldList = binary:split(Current, [<<?SOH>>], [global]),
    ParsedMessage = lists:map(fun(Elem) -> field_parse(Elem) end, FieldList),
    {ok, {ParsedMessage, Rest}}.
    
field_parse(<<"1=", Value/binary>>) -> 
{'Account', Value};
field_parse(<<"2=", Value/binary>>) -> 
{'AdvId', Value};
field_parse(<<"3=", Value/binary>>) -> 
{'AdvRefID', Value};
field_parse(<<"4=", Value/binary>>) -> 
Val = case Value of
<<"B">> -> 'BUY'; 
<<"S">> -> 'SELL'; 
<<"T">> -> 'TRADE'; 
<<"X">> -> 'CROSS'; 
_ -> unknown
end,
{'AdvSide', Val};
field_parse(<<"5=", Value/binary>>) -> 
Val = case Value of
<<"C">> -> 'CANCEL'; 
<<"N">> -> 'NEW'; 
<<"R">> -> 'REPLACE'; 
_ -> unknown
end,
{'AdvTransType', Val};
field_parse(<<"6=", Value/binary>>) -> 
{'AvgPx', to_float(Value)};
field_parse(<<"7=", Value/binary>>) -> 
{'BeginSeqNo', to_int(Value)};
field_parse(<<"8=", Value/binary>>) -> 
{'BeginString', Value};
field_parse(<<"9=", Value/binary>>) -> 
{'BodyLength', to_int(Value)};
field_parse(<<"10=", Value/binary>>) -> 
{'CheckSum', Value};
field_parse(<<"11=", Value/binary>>) -> 
{'ClOrdID', Value};
field_parse(<<"12=", Value/binary>>) -> 
{'Commission', Value};
field_parse(<<"13=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'PER_SHARE'; 
<<"2">> -> 'PERCENTAGE'; 
<<"3">> -> 'ABSOLUTE'; 
_ -> unknown
end,
{'CommType', Val};
field_parse(<<"14=", Value/binary>>) -> 
{'CumQty', to_float(Value)};
field_parse(<<"15=", Value/binary>>) -> 
{'Currency', to_float(Value)};
field_parse(<<"16=", Value/binary>>) -> 
{'EndSeqNo', to_int(Value)};
field_parse(<<"17=", Value/binary>>) -> 
{'ExecID', Value};
field_parse(<<"18=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'STAY_ON_OFFERSIDE'; 
<<"1">> -> 'NOT_HELD'; 
<<"2">> -> 'WORK'; 
<<"3">> -> 'GO_ALONG'; 
<<"4">> -> 'OVER_THE_DAY'; 
<<"5">> -> 'HELD'; 
<<"6">> -> 'PARTICIPATE_DONT_INITIATE'; 
<<"7">> -> 'STRICT_SCALE'; 
<<"8">> -> 'TRY_TO_SCALE'; 
<<"9">> -> 'STAY_ON_BIDSIDE'; 
<<"A">> -> 'NO_CROSS'; 
<<"B">> -> 'OK_TO_CROSS'; 
<<"C">> -> 'CALL_FIRST'; 
<<"D">> -> 'PERCENT_OF_VOLUME'; 
<<"E">> -> 'DO_NOT_INCREASE'; 
<<"F">> -> 'DO_NOT_REDUCE'; 
<<"G">> -> 'ALL_OR_NONE'; 
<<"I">> -> 'INSTITUTIONS_ONLY'; 
<<"L">> -> 'LAST_PEG'; 
<<"M">> -> 'MID_PRICE_PEG'; 
<<"N">> -> 'NON_NEGOTIABLE'; 
<<"O">> -> 'OPENING_PEG'; 
<<"P">> -> 'MARKET_PEG'; 
<<"R">> -> 'PRIMARY_PEG'; 
<<"S">> -> 'SUSPEND'; 
<<"T">> -> 'FIXED_PEG_TO_LOCAL_BEST_BID_OR_OFFER_AT_TIME_OF_ORDER'; 
<<"U">> -> 'CUSTOMER_DISPLAY_INSTRUCTION'; 
<<"V">> -> 'NETTING'; 
<<"W">> -> 'PEG_TO_VWAP'; 
_ -> unknown
end,
{'ExecInst', Val};
field_parse(<<"19=", Value/binary>>) -> 
{'ExecRefID', Value};
field_parse(<<"20=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NEW'; 
<<"1">> -> 'CANCEL'; 
<<"2">> -> 'CORRECT'; 
<<"3">> -> 'STATUS'; 
_ -> unknown
end,
{'ExecTransType', Val};
field_parse(<<"21=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'AUTOMATED_EXECUTION_ORDER_PRIVATE_NO_BROKER_INTERVENTION'; 
<<"2">> -> 'AUTOMATED_EXECUTION_ORDER_PUBLIC_BROKER_INTERVENTION_OK'; 
<<"3">> -> 'MANUAL_ORDER_BEST_EXECUTION'; 
_ -> unknown
end,
{'HandlInst', Val};
field_parse(<<"22=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'CUSIP'; 
<<"2">> -> 'SEDOL'; 
<<"3">> -> 'QUIK'; 
<<"4">> -> 'ISIN_NUMBER'; 
<<"5">> -> 'RIC_CODE'; 
<<"6">> -> 'ISO_CURRENCY_CODE'; 
<<"7">> -> 'ISO_COUNTRY_CODE'; 
<<"8">> -> 'EXCHANGE_SYMBOL'; 
<<"9">> -> 'CONSOLIDATED_TAPE_ASSOCIATION'; 
_ -> unknown
end,
{'IDSource', Val};
field_parse(<<"23=", Value/binary>>) -> 
{'IOIid', Value};
field_parse(<<"24=", Value/binary>>) -> 
{'IOIOthSvc', Value};
field_parse(<<"25=", Value/binary>>) -> 
Val = case Value of
<<"H">> -> 'HIGH'; 
<<"L">> -> 'LOW'; 
<<"M">> -> 'MEDIUM'; 
_ -> unknown
end,
{'IOIQltyInd', Val};
field_parse(<<"26=", Value/binary>>) -> 
{'IOIRefID', Value};
field_parse(<<"27=", Value/binary>>) -> 
Val = case Value of
<<"L">> -> 'LARGE'; 
<<"M">> -> 'MEDIUM'; 
<<"S">> -> 'SMALL'; 
_ -> unknown
end,
{'IOIShares', Val};
field_parse(<<"28=", Value/binary>>) -> 
Val = case Value of
<<"C">> -> 'CANCEL'; 
<<"N">> -> 'NEW'; 
<<"R">> -> 'REPLACE'; 
_ -> unknown
end,
{'IOITransType', Val};
field_parse(<<"29=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'AGENT'; 
<<"2">> -> 'CROSS_AS_AGENT'; 
<<"3">> -> 'CROSS_AS_PRINCIPAL'; 
<<"4">> -> 'PRINCIPAL'; 
_ -> unknown
end,
{'LastCapacity', Val};
field_parse(<<"30=", Value/binary>>) -> 
{'LastMkt', Value};
field_parse(<<"31=", Value/binary>>) -> 
{'LastPx', to_float(Value)};
field_parse(<<"32=", Value/binary>>) -> 
{'LastShares', to_float(Value)};
field_parse(<<"33=", Value/binary>>) -> 
{'LinesOfText', to_int(Value)};
field_parse(<<"34=", Value/binary>>) -> 
{'MsgSeqNum', to_int(Value)};
field_parse(<<"35=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'HEARTBEAT'; 
<<"1">> -> 'TEST_REQUEST'; 
<<"2">> -> 'RESEND_REQUEST'; 
<<"3">> -> 'REJECT'; 
<<"4">> -> 'SEQUENCE_RESET'; 
<<"5">> -> 'LOGOUT'; 
<<"6">> -> 'INDICATION_OF_INTEREST'; 
<<"7">> -> 'ADVERTISEMENT'; 
<<"8">> -> 'EXECUTION_REPORT'; 
<<"9">> -> 'ORDER_CANCEL_REJECT'; 
<<"a">> -> 'QUOTE_STATUS_REQUEST'; 
<<"A">> -> 'LOGON'; 
<<"B">> -> 'NEWS'; 
<<"b">> -> 'QUOTE_ACKNOWLEDGEMENT'; 
<<"C">> -> 'EMAIL'; 
<<"c">> -> 'SECURITY_DEFINITION_REQUEST'; 
<<"D">> -> 'ORDER_SINGLE'; 
<<"d">> -> 'SECURITY_DEFINITION'; 
<<"E">> -> 'ORDER_LIST'; 
<<"e">> -> 'SECURITY_STATUS_REQUEST'; 
<<"f">> -> 'SECURITY_STATUS'; 
<<"F">> -> 'ORDER_CANCEL_REQUEST'; 
<<"G">> -> 'ORDER_CANCEL_REPLACE_REQUEST'; 
<<"g">> -> 'TRADING_SESSION_STATUS_REQUEST'; 
<<"H">> -> 'ORDER_STATUS_REQUEST'; 
<<"h">> -> 'TRADING_SESSION_STATUS'; 
<<"i">> -> 'MASS_QUOTE'; 
<<"j">> -> 'BUSINESS_MESSAGE_REJECT'; 
<<"J">> -> 'ALLOCATION'; 
<<"K">> -> 'LIST_CANCEL_REQUEST'; 
<<"k">> -> 'BID_REQUEST'; 
<<"l">> -> 'BID_RESPONSE'; 
<<"L">> -> 'LIST_EXECUTE'; 
<<"m">> -> 'LIST_STRIKE_PRICE'; 
<<"M">> -> 'LIST_STATUS_REQUEST'; 
<<"N">> -> 'LIST_STATUS'; 
<<"P">> -> 'ALLOCATION_ACK'; 
<<"Q">> -> 'DONT_KNOW_TRADE'; 
<<"R">> -> 'QUOTE_REQUEST'; 
<<"S">> -> 'QUOTE'; 
<<"T">> -> 'SETTLEMENT_INSTRUCTIONS'; 
<<"V">> -> 'MARKET_DATA_REQUEST'; 
<<"W">> -> 'MARKET_DATA_SNAPSHOT_FULL_REFRESH'; 
<<"X">> -> 'MARKET_DATA_INCREMENTAL_REFRESH'; 
<<"Y">> -> 'MARKET_DATA_REQUEST_REJECT'; 
<<"Z">> -> 'QUOTE_CANCEL'; 
_ -> unknown
end,
{'MsgType', Val};
field_parse(<<"36=", Value/binary>>) -> 
{'NewSeqNo', to_int(Value)};
field_parse(<<"37=", Value/binary>>) -> 
{'OrderID', Value};
field_parse(<<"38=", Value/binary>>) -> 
{'OrderQty', to_float(Value)};
field_parse(<<"39=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NEW'; 
<<"1">> -> 'PARTIALLY_FILLED'; 
<<"2">> -> 'FILLED'; 
<<"3">> -> 'DONE_FOR_DAY'; 
<<"4">> -> 'CANCELED'; 
<<"5">> -> 'REPLACED'; 
<<"6">> -> 'PENDING_CANCEL'; 
<<"7">> -> 'STOPPED'; 
<<"8">> -> 'REJECTED'; 
<<"9">> -> 'SUSPENDED'; 
<<"A">> -> 'PENDING_NEW'; 
<<"B">> -> 'CALCULATED'; 
<<"C">> -> 'EXPIRED'; 
<<"D">> -> 'ACCEPTED_FOR_BIDDING'; 
<<"E">> -> 'PENDING_REPLACE'; 
_ -> unknown
end,
{'OrdStatus', Val};
field_parse(<<"40=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'MARKET'; 
<<"2">> -> 'LIMIT'; 
<<"3">> -> 'STOP'; 
<<"4">> -> 'STOP_LIMIT'; 
<<"5">> -> 'MARKET_ON_CLOSE'; 
<<"6">> -> 'WITH_OR_WITHOUT'; 
<<"7">> -> 'LIMIT_OR_BETTER'; 
<<"8">> -> 'LIMIT_WITH_OR_WITHOUT'; 
<<"9">> -> 'ON_BASIS'; 
<<"A">> -> 'ON_CLOSE'; 
<<"B">> -> 'LIMIT_ON_CLOSE'; 
<<"C">> -> 'FOREX_C'; 
<<"D">> -> 'PREVIOUSLY_QUOTED'; 
<<"E">> -> 'PREVIOUSLY_INDICATED'; 
<<"F">> -> 'FOREX_F'; 
<<"G">> -> 'FOREX_G'; 
<<"H">> -> 'FOREX_H'; 
<<"I">> -> 'FUNARI'; 
<<"P">> -> 'PEGGED'; 
_ -> unknown
end,
{'OrdType', Val};
field_parse(<<"41=", Value/binary>>) -> 
{'OrigClOrdID', Value};
field_parse(<<"42=", Value/binary>>) -> 
{'OrigTime', Value};
field_parse(<<"43=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'PossDupFlag', Val};
field_parse(<<"44=", Value/binary>>) -> 
{'Price', to_float(Value)};
field_parse(<<"45=", Value/binary>>) -> 
{'RefSeqNum', to_int(Value)};
field_parse(<<"46=", Value/binary>>) -> 
{'RelatdSym', Value};
field_parse(<<"47=", Value/binary>>) -> 
Val = case Value of
<<"A">> -> 'AGENCY_SINGLE_ORDER'; 
<<"B">> -> 'SHORT_EXEMPT_TRANSACTION_B'; 
<<"C">> -> 'PROGRAM_ORDER_NON_INDEX_ARB_FOR_MEMBER_FIRM_ORG'; 
<<"D">> -> 'PROGRAM_ORDER_INDEX_ARB_FOR_MEMBER_FIRM_ORG'; 
<<"E">> -> 'REGISTERED_EQUITY_MARKET_MAKER_TRADES'; 
<<"F">> -> 'SHORT_EXEMPT_TRANSACTION_F'; 
<<"H">> -> 'SHORT_EXEMPT_TRANSACTION_H'; 
<<"I">> -> 'INDIVIDUAL_INVESTOR_SINGLE_ORDER'; 
<<"J">> -> 'PROGRAM_ORDER_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER'; 
<<"K">> -> 'PROGRAM_ORDER_NON_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER'; 
<<"L">> -> 'SHORT_EXEMPT_TRANSACTION_FOR_MEMBER_COMPETING_MARKET_MAKER_AFFILIATED_WITH_THE_FIRM_CLEARING_THE_TRADE'; 
<<"M">> -> 'PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_MEMBER'; 
<<"N">> -> 'PROGRAM_ORDER_NON_INDEX_ARB_FOR_OTHER_MEMBER'; 
<<"O">> -> 'COMPETING_DEALER_TRADES_O'; 
<<"P">> -> 'PRINCIPAL'; 
<<"R">> -> 'COMPETING_DEALER_TRADES_R'; 
<<"S">> -> 'SPECIALIST_TRADES'; 
<<"T">> -> 'COMPETING_DEALER_TRADES_T'; 
<<"U">> -> 'PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_AGENCY'; 
<<"W">> -> 'ALL_OTHER_ORDERS_AS_AGENT_FOR_OTHER_MEMBER'; 
<<"X">> -> 'SHORT_EXEMPT_TRANSACTION_FOR_MEMBER_COMPETING_MARKET_MAKER_NOT_AFFILIATED_WITH_THE_FIRM_CLEARING_THE_TRADE'; 
<<"Y">> -> 'PROGRAM_ORDER_NON_INDEX_ARB_FOR_OTHER_AGENCY'; 
<<"Z">> -> 'SHORT_EXEMPT_TRANSACTION_FOR_NON_MEMBER_COMPETING_MARKET_MAKER'; 
_ -> unknown
end,
{'Rule80A', Val};
field_parse(<<"48=", Value/binary>>) -> 
{'SecurityID', Value};
field_parse(<<"49=", Value/binary>>) -> 
{'SenderCompID', Value};
field_parse(<<"50=", Value/binary>>) -> 
{'SenderSubID', Value};
field_parse(<<"51=", Value/binary>>) -> 
{'SendingDate', Value};
field_parse(<<"52=", Value/binary>>) -> 
{'SendingTime', Value};
field_parse(<<"53=", Value/binary>>) -> 
{'Shares', to_float(Value)};
field_parse(<<"54=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'BUY'; 
<<"2">> -> 'SELL'; 
<<"3">> -> 'BUY_MINUS'; 
<<"4">> -> 'SELL_PLUS'; 
<<"5">> -> 'SELL_SHORT'; 
<<"6">> -> 'SELL_SHORT_EXEMPT'; 
<<"7">> -> 'UNDISCLOSED'; 
<<"8">> -> 'CROSS'; 
<<"9">> -> 'CROSS_SHORT'; 
_ -> unknown
end,
{'Side', Val};
field_parse(<<"55=", Value/binary>>) -> 
{'Symbol', Value};
field_parse(<<"56=", Value/binary>>) -> 
{'TargetCompID', Value};
field_parse(<<"57=", Value/binary>>) -> 
{'TargetSubID', Value};
field_parse(<<"58=", Value/binary>>) -> 
{'Text', Value};
field_parse(<<"59=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'DAY'; 
<<"1">> -> 'GOOD_TILL_CANCEL'; 
<<"2">> -> 'AT_THE_OPENING'; 
<<"3">> -> 'IMMEDIATE_OR_CANCEL'; 
<<"4">> -> 'FILL_OR_KILL'; 
<<"5">> -> 'GOOD_TILL_CROSSING'; 
<<"6">> -> 'GOOD_TILL_DATE'; 
_ -> unknown
end,
{'TimeInForce', Val};
field_parse(<<"60=", Value/binary>>) -> 
{'TransactTime', Value};
field_parse(<<"61=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NORMAL'; 
<<"1">> -> 'FLASH'; 
<<"2">> -> 'BACKGROUND'; 
_ -> unknown
end,
{'Urgency', Val};
field_parse(<<"62=", Value/binary>>) -> 
{'ValidUntilTime', Value};
field_parse(<<"63=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'REGULAR'; 
<<"1">> -> 'CASH'; 
<<"2">> -> 'NEXT_DAY'; 
<<"3">> -> 'T_PLUS_2'; 
<<"4">> -> 'T_PLUS_3'; 
<<"5">> -> 'T_PLUS_4'; 
<<"6">> -> 'FUTURE'; 
<<"7">> -> 'WHEN_ISSUED'; 
<<"8">> -> 'SELLERS_OPTION'; 
<<"9">> -> 'T_PLUS_5'; 
_ -> unknown
end,
{'SettlmntTyp', Val};
field_parse(<<"64=", Value/binary>>) -> 
{'FutSettDate', Value};
field_parse(<<"65=", Value/binary>>) -> 
{'SymbolSfx', Value};
field_parse(<<"66=", Value/binary>>) -> 
{'ListID', Value};
field_parse(<<"67=", Value/binary>>) -> 
{'ListSeqNo', to_int(Value)};
field_parse(<<"68=", Value/binary>>) -> 
{'TotNoOrders', to_int(Value)};
field_parse(<<"69=", Value/binary>>) -> 
{'ListExecInst', Value};
field_parse(<<"70=", Value/binary>>) -> 
{'AllocID', Value};
field_parse(<<"71=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NEW'; 
<<"1">> -> 'REPLACE'; 
<<"2">> -> 'CANCEL'; 
<<"3">> -> 'PRELIMINARY'; 
<<"4">> -> 'CALCULATED'; 
<<"5">> -> 'CALCULATED_WITHOUT_PRELIMINARY'; 
_ -> unknown
end,
{'AllocTransType', Val};
field_parse(<<"72=", Value/binary>>) -> 
{'RefAllocID', Value};
field_parse(<<"73=", Value/binary>>) -> 
{'NoOrders', to_int(Value)};
field_parse(<<"74=", Value/binary>>) -> 
{'AvgPrxPrecision', to_int(Value)};
field_parse(<<"75=", Value/binary>>) -> 
{'TradeDate', Value};
field_parse(<<"76=", Value/binary>>) -> 
{'ExecBroker', Value};
field_parse(<<"77=", Value/binary>>) -> 
Val = case Value of
<<"C">> -> 'CLOSE'; 
<<"O">> -> 'OPEN'; 
_ -> unknown
end,
{'OpenClose', Val};
field_parse(<<"78=", Value/binary>>) -> 
{'NoAllocs', to_int(Value)};
field_parse(<<"79=", Value/binary>>) -> 
{'AllocAccount', Value};
field_parse(<<"80=", Value/binary>>) -> 
{'AllocShares', to_float(Value)};
field_parse(<<"81=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'REGULAR'; 
<<"1">> -> 'SOFT_DOLLAR'; 
<<"2">> -> 'STEP_IN'; 
<<"3">> -> 'STEP_OUT'; 
<<"4">> -> 'SOFT_DOLLAR_STEP_IN'; 
<<"5">> -> 'SOFT_DOLLAR_STEP_OUT'; 
<<"6">> -> 'PLAN_SPONSOR'; 
_ -> unknown
end,
{'ProcessCode', Val};
field_parse(<<"82=", Value/binary>>) -> 
{'NoRpts', to_int(Value)};
field_parse(<<"83=", Value/binary>>) -> 
{'RptSeq', to_int(Value)};
field_parse(<<"84=", Value/binary>>) -> 
{'CxlQty', to_float(Value)};
field_parse(<<"85=", Value/binary>>) -> 
{'NoDlvyInst', to_int(Value)};
field_parse(<<"86=", Value/binary>>) -> 
{'DlvyInst', Value};
field_parse(<<"87=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'ACCEPTED'; 
<<"1">> -> 'REJECTED'; 
<<"2">> -> 'PARTIAL_ACCEPT'; 
<<"3">> -> 'RECEIVED'; 
_ -> unknown
end,
{'AllocStatus', Val};
field_parse(<<"88=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'UNKNOWN_ACCOUNT'; 
<<"1">> -> 'INCORRECT_QUANTITY'; 
<<"2">> -> 'INCORRECT_AVERAGE_PRICE'; 
<<"3">> -> 'UNKNOWN_EXECUTING_BROKER_MNEMONIC'; 
<<"4">> -> 'COMMISSION_DIFFERENCE'; 
<<"5">> -> 'UNKNOWN_ORDERID'; 
<<"6">> -> 'UNKNOWN_LISTID'; 
<<"7">> -> 'OTHER'; 
_ -> unknown
end,
{'AllocRejCode', Val};
field_parse(<<"89=", Value/binary>>) -> 
{'Signature', Value};
field_parse(<<"90=", Value/binary>>) -> 
{'SecureDataLen', to_int(Value)};
field_parse(<<"91=", Value/binary>>) -> 
{'SecureData', Value};
field_parse(<<"92=", Value/binary>>) -> 
{'BrokerOfCredit', Value};
field_parse(<<"93=", Value/binary>>) -> 
{'SignatureLength', to_int(Value)};
field_parse(<<"94=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NEW'; 
<<"1">> -> 'REPLY'; 
<<"2">> -> 'ADMIN_REPLY'; 
_ -> unknown
end,
{'EmailType', Val};
field_parse(<<"95=", Value/binary>>) -> 
{'RawDataLength', to_int(Value)};
field_parse(<<"96=", Value/binary>>) -> 
{'RawData', Value};
field_parse(<<"97=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'PossResend', Val};
field_parse(<<"98=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NONE'; 
<<"1">> -> 'PKCS'; 
<<"2">> -> 'DES'; 
<<"3">> -> 'PKCS_DES'; 
<<"4">> -> 'PGP_DES'; 
<<"5">> -> 'PGP_DES_MD5'; 
<<"6">> -> 'PEM_DES_MD5'; 
_ -> unknown
end,
{'EncryptMethod', Val};
field_parse(<<"99=", Value/binary>>) -> 
{'StopPx', to_float(Value)};
field_parse(<<"100=", Value/binary>>) -> 
{'ExDestination', Value};
field_parse(<<"102=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'TOO_LATE_TO_CANCEL'; 
<<"1">> -> 'UNKNOWN_ORDER'; 
<<"2">> -> 'BROKER_OPTION'; 
<<"3">> -> 'ORDER_ALREADY_IN_PENDING_CANCEL_OR_PENDING_REPLACE_STATUS'; 
_ -> unknown
end,
{'CxlRejReason', Val};
field_parse(<<"103=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'BROKER_OPTION'; 
<<"1">> -> 'UNKNOWN_SYMBOL'; 
<<"2">> -> 'EXCHANGE_CLOSED'; 
<<"3">> -> 'ORDER_EXCEEDS_LIMIT'; 
<<"4">> -> 'TOO_LATE_TO_ENTER'; 
<<"5">> -> 'UNKNOWN_ORDER'; 
<<"6">> -> 'DUPLICATE_ORDER'; 
<<"7">> -> 'DUPLICATE_OF_A_VERBALLY_COMMUNICATED_ORDER'; 
<<"8">> -> 'STALE_ORDER'; 
_ -> unknown
end,
{'OrdRejReason', Val};
field_parse(<<"104=", Value/binary>>) -> 
Val = case Value of
<<"A">> -> 'ALL_OR_NONE'; 
<<"C">> -> 'AT_THE_CLOSE'; 
<<"I">> -> 'IN_TOUCH_WITH'; 
<<"L">> -> 'LIMIT'; 
<<"M">> -> 'MORE_BEHIND'; 
<<"O">> -> 'AT_THE_OPEN'; 
<<"P">> -> 'TAKING_A_POSITION'; 
<<"Q">> -> 'AT_THE_MARKET'; 
<<"R">> -> 'READY_TO_TRADE'; 
<<"S">> -> 'PORTFOLIO_SHOW_N'; 
<<"T">> -> 'THROUGH_THE_DAY'; 
<<"V">> -> 'VERSUS'; 
<<"W">> -> 'INDICATION'; 
<<"X">> -> 'CROSSING_OPPORTUNITY'; 
<<"Y">> -> 'AT_THE_MIDPOINT'; 
<<"Z">> -> 'PRE_OPEN'; 
_ -> unknown
end,
{'IOIQualifier', Val};
field_parse(<<"105=", Value/binary>>) -> 
{'WaveNo', Value};
field_parse(<<"106=", Value/binary>>) -> 
{'Issuer', Value};
field_parse(<<"107=", Value/binary>>) -> 
{'SecurityDesc', Value};
field_parse(<<"108=", Value/binary>>) -> 
{'HeartBtInt', to_int(Value)};
field_parse(<<"109=", Value/binary>>) -> 
{'ClientID', Value};
field_parse(<<"110=", Value/binary>>) -> 
{'MinQty', to_float(Value)};
field_parse(<<"111=", Value/binary>>) -> 
{'MaxFloor', to_float(Value)};
field_parse(<<"112=", Value/binary>>) -> 
{'TestReqID', Value};
field_parse(<<"113=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'ReportToExch', Val};
field_parse(<<"114=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'LocateReqd', Val};
field_parse(<<"115=", Value/binary>>) -> 
{'OnBehalfOfCompID', Value};
field_parse(<<"116=", Value/binary>>) -> 
{'OnBehalfOfSubID', Value};
field_parse(<<"117=", Value/binary>>) -> 
{'QuoteID', Value};
field_parse(<<"118=", Value/binary>>) -> 
{'NetMoney', Value};
field_parse(<<"119=", Value/binary>>) -> 
{'SettlCurrAmt', Value};
field_parse(<<"120=", Value/binary>>) -> 
{'SettlCurrency', to_float(Value)};
field_parse(<<"121=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'ForexReq', Val};
field_parse(<<"122=", Value/binary>>) -> 
{'OrigSendingTime', Value};
field_parse(<<"123=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'GapFillFlag', Val};
field_parse(<<"124=", Value/binary>>) -> 
{'NoExecs', to_int(Value)};
field_parse(<<"125=", Value/binary>>) -> 
{'CxlType', Value};
field_parse(<<"126=", Value/binary>>) -> 
{'ExpireTime', Value};
field_parse(<<"127=", Value/binary>>) -> 
Val = case Value of
<<"A">> -> 'UNKNOWN_SYMBOL'; 
<<"B">> -> 'WRONG_SIDE'; 
<<"C">> -> 'QUANTITY_EXCEEDS_ORDER'; 
<<"D">> -> 'NO_MATCHING_ORDER'; 
<<"E">> -> 'PRICE_EXCEEDS_LIMIT'; 
<<"Z">> -> 'OTHER'; 
_ -> unknown
end,
{'DKReason', Val};
field_parse(<<"128=", Value/binary>>) -> 
{'DeliverToCompID', Value};
field_parse(<<"129=", Value/binary>>) -> 
{'DeliverToSubID', Value};
field_parse(<<"130=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'IOINaturalFlag', Val};
field_parse(<<"131=", Value/binary>>) -> 
{'QuoteReqID', Value};
field_parse(<<"132=", Value/binary>>) -> 
{'BidPx', to_float(Value)};
field_parse(<<"133=", Value/binary>>) -> 
{'OfferPx', to_float(Value)};
field_parse(<<"134=", Value/binary>>) -> 
{'BidSize', to_float(Value)};
field_parse(<<"135=", Value/binary>>) -> 
{'OfferSize', to_float(Value)};
field_parse(<<"136=", Value/binary>>) -> 
{'NoMiscFees', to_int(Value)};
field_parse(<<"137=", Value/binary>>) -> 
{'MiscFeeAmt', Value};
field_parse(<<"138=", Value/binary>>) -> 
{'MiscFeeCurr', to_float(Value)};
field_parse(<<"139=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'REGULATORY'; 
<<"2">> -> 'TAX'; 
<<"3">> -> 'LOCAL_COMMISSION'; 
<<"4">> -> 'EXCHANGE_FEES'; 
<<"5">> -> 'STAMP'; 
<<"6">> -> 'LEVY'; 
<<"7">> -> 'OTHER'; 
<<"8">> -> 'MARKUP'; 
<<"9">> -> 'CONSUMPTION_TAX'; 
_ -> unknown
end,
{'MiscFeeType', Val};
field_parse(<<"140=", Value/binary>>) -> 
{'PrevClosePx', to_float(Value)};
field_parse(<<"141=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'ResetSeqNumFlag', Val};
field_parse(<<"142=", Value/binary>>) -> 
{'SenderLocationID', Value};
field_parse(<<"143=", Value/binary>>) -> 
{'TargetLocationID', Value};
field_parse(<<"144=", Value/binary>>) -> 
{'OnBehalfOfLocationID', Value};
field_parse(<<"145=", Value/binary>>) -> 
{'DeliverToLocationID', Value};
field_parse(<<"146=", Value/binary>>) -> 
{'NoRelatedSym', to_int(Value)};
field_parse(<<"147=", Value/binary>>) -> 
{'Subject', Value};
field_parse(<<"148=", Value/binary>>) -> 
{'Headline', Value};
field_parse(<<"149=", Value/binary>>) -> 
{'URLLink', Value};
field_parse(<<"150=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NEW'; 
<<"1">> -> 'PARTIAL_FILL'; 
<<"2">> -> 'FILL'; 
<<"3">> -> 'DONE_FOR_DAY'; 
<<"4">> -> 'CANCELED'; 
<<"5">> -> 'REPLACE'; 
<<"6">> -> 'PENDING_CANCEL'; 
<<"7">> -> 'STOPPED'; 
<<"8">> -> 'REJECTED'; 
<<"9">> -> 'SUSPENDED'; 
<<"A">> -> 'PENDING_NEW'; 
<<"B">> -> 'CALCULATED'; 
<<"C">> -> 'EXPIRED'; 
<<"D">> -> 'RESTATED'; 
<<"E">> -> 'PENDING_REPLACE'; 
_ -> unknown
end,
{'ExecType', Val};
field_parse(<<"151=", Value/binary>>) -> 
{'LeavesQty', to_float(Value)};
field_parse(<<"152=", Value/binary>>) -> 
{'CashOrderQty', to_float(Value)};
field_parse(<<"153=", Value/binary>>) -> 
{'AllocAvgPx', to_float(Value)};
field_parse(<<"154=", Value/binary>>) -> 
{'AllocNetMoney', Value};
field_parse(<<"155=", Value/binary>>) -> 
{'SettlCurrFxRate', Value};
field_parse(<<"156=", Value/binary>>) -> 
Val = case Value of
<<"M">> -> 'MULTIPLY'; 
<<"D">> -> 'DIVIDE'; 
_ -> unknown
end,
{'SettlCurrFxRateCalc', Val};
field_parse(<<"157=", Value/binary>>) -> 
{'NumDaysInterest', to_int(Value)};
field_parse(<<"158=", Value/binary>>) -> 
{'AccruedInterestRate', Value};
field_parse(<<"159=", Value/binary>>) -> 
{'AccruedInterestAmt', Value};
field_parse(<<"160=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'DEFAULT'; 
<<"1">> -> 'STANDING_INSTRUCTIONS_PROVIDED'; 
<<"2">> -> 'SPECIFIC_ALLOCATION_ACCOUNT_OVERRIDING'; 
<<"3">> -> 'SPECIFIC_ALLOCATION_ACCOUNT_STANDING'; 
_ -> unknown
end,
{'SettlInstMode', Val};
field_parse(<<"161=", Value/binary>>) -> 
{'AllocText', Value};
field_parse(<<"162=", Value/binary>>) -> 
{'SettlInstID', Value};
field_parse(<<"163=", Value/binary>>) -> 
Val = case Value of
<<"C">> -> 'CANCEL'; 
<<"N">> -> 'NEW'; 
<<"R">> -> 'REPLACE'; 
_ -> unknown
end,
{'SettlInstTransType', Val};
field_parse(<<"164=", Value/binary>>) -> 
{'EmailThreadID', Value};
field_parse(<<"165=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'BROKERS_INSTRUCTIONS'; 
<<"2">> -> 'INSTITUTIONS_INSTRUCTIONS'; 
_ -> unknown
end,
{'SettlInstSource', Val};
field_parse(<<"166=", Value/binary>>) -> 
Val = case Value of
<<"CED">> -> 'CEDEL'; 
<<"DTC">> -> 'DEPOSITORY_TRUST_COMPANY'; 
<<"EUR">> -> 'EUROCLEAR'; 
<<"FED">> -> 'FEDERAL_BOOK_ENTRY'; 
<<"ISO Country Code">> -> 'LOCAL_MARKET_SETTLE_LOCATION'; 
<<"PNY">> -> 'PHYSICAL'; 
<<"PTC">> -> 'PARTICIPANT_TRUST_COMPANY'; 
_ -> unknown
end,
{'SettlLocation', Val};
field_parse(<<"167=", Value/binary>>) -> 
Val = case Value of
<<"?">> -> 'WILDCARD_ENTRY'; 
<<"BA">> -> 'BANKERS_ACCEPTANCE'; 
<<"CB">> -> 'CONVERTIBLE_BOND'; 
<<"CD">> -> 'CERTIFICATE_OF_DEPOSIT'; 
<<"CMO">> -> 'COLLATERALIZE_MORTGAGE_OBLIGATION'; 
<<"CORP">> -> 'CORPORATE_BOND'; 
<<"CP">> -> 'COMMERCIAL_PAPER'; 
<<"CPP">> -> 'CORPORATE_PRIVATE_PLACEMENT'; 
<<"CS">> -> 'COMMON_STOCK'; 
<<"FHA">> -> 'FEDERAL_HOUSING_AUTHORITY'; 
<<"FHL">> -> 'FEDERAL_HOME_LOAN'; 
<<"FN">> -> 'FEDERAL_NATIONAL_MORTGAGE_ASSOCIATION'; 
<<"FOR">> -> 'FOREIGN_EXCHANGE_CONTRACT'; 
<<"FUT">> -> 'FUTURE'; 
<<"GN">> -> 'GOVERNMENT_NATIONAL_MORTGAGE_ASSOCIATION'; 
<<"GOVT">> -> 'TREASURIES_PLUS_AGENCY_DEBENTURE'; 
<<"IET">> -> 'MORTGAGE_IOETTE'; 
<<"MF">> -> 'MUTUAL_FUND'; 
<<"MIO">> -> 'MORTGAGE_INTEREST_ONLY'; 
<<"MPO">> -> 'MORTGAGE_PRINCIPAL_ONLY'; 
<<"MPP">> -> 'MORTGAGE_PRIVATE_PLACEMENT'; 
<<"MPT">> -> 'MISCELLANEOUS_PASS_THRU'; 
<<"MUNI">> -> 'MUNICIPAL_BOND'; 
<<"NONE">> -> 'NO_ISITC_SECURITY_TYPE'; 
<<"OPT">> -> 'OPTION'; 
<<"PS">> -> 'PREFERRED_STOCK'; 
<<"RP">> -> 'REPURCHASE_AGREEMENT'; 
<<"RVRP">> -> 'REVERSE_REPURCHASE_AGREEMENT'; 
<<"SL">> -> 'STUDENT_LOAN_MARKETING_ASSOCIATION'; 
<<"TD">> -> 'TIME_DEPOSIT'; 
<<"USTB">> -> 'US_TREASURY_BILL'; 
<<"WAR">> -> 'WARRANT'; 
<<"ZOO">> -> 'CATS_TIGERS_LIONS'; 
_ -> unknown
end,
{'SecurityType', Val};
field_parse(<<"168=", Value/binary>>) -> 
{'EffectiveTime', Value};
field_parse(<<"169=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'OTHER'; 
<<"1">> -> 'DTC_SID'; 
<<"2">> -> 'THOMSON_ALERT'; 
<<"3">> -> 'A_GLOBAL_CUSTODIAN'; 
_ -> unknown
end,
{'StandInstDbType', Val};
field_parse(<<"170=", Value/binary>>) -> 
{'StandInstDbName', Value};
field_parse(<<"171=", Value/binary>>) -> 
{'StandInstDbID', Value};
field_parse(<<"172=", Value/binary>>) -> 
{'SettlDeliveryType', to_int(Value)};
field_parse(<<"173=", Value/binary>>) -> 
{'SettlDepositoryCode', Value};
field_parse(<<"174=", Value/binary>>) -> 
{'SettlBrkrCode', Value};
field_parse(<<"175=", Value/binary>>) -> 
{'SettlInstCode', Value};
field_parse(<<"176=", Value/binary>>) -> 
{'SecuritySettlAgentName', Value};
field_parse(<<"177=", Value/binary>>) -> 
{'SecuritySettlAgentCode', Value};
field_parse(<<"178=", Value/binary>>) -> 
{'SecuritySettlAgentAcctNum', Value};
field_parse(<<"179=", Value/binary>>) -> 
{'SecuritySettlAgentAcctName', Value};
field_parse(<<"180=", Value/binary>>) -> 
{'SecuritySettlAgentContactName', Value};
field_parse(<<"181=", Value/binary>>) -> 
{'SecuritySettlAgentContactPhone', Value};
field_parse(<<"182=", Value/binary>>) -> 
{'CashSettlAgentName', Value};
field_parse(<<"183=", Value/binary>>) -> 
{'CashSettlAgentCode', Value};
field_parse(<<"184=", Value/binary>>) -> 
{'CashSettlAgentAcctNum', Value};
field_parse(<<"185=", Value/binary>>) -> 
{'CashSettlAgentAcctName', Value};
field_parse(<<"186=", Value/binary>>) -> 
{'CashSettlAgentContactName', Value};
field_parse(<<"187=", Value/binary>>) -> 
{'CashSettlAgentContactPhone', Value};
field_parse(<<"188=", Value/binary>>) -> 
{'BidSpotRate', to_float(Value)};
field_parse(<<"189=", Value/binary>>) -> 
{'BidForwardPoints', Value};
field_parse(<<"190=", Value/binary>>) -> 
{'OfferSpotRate', to_float(Value)};
field_parse(<<"191=", Value/binary>>) -> 
{'OfferForwardPoints', Value};
field_parse(<<"192=", Value/binary>>) -> 
{'OrderQty2', to_float(Value)};
field_parse(<<"193=", Value/binary>>) -> 
{'FutSettDate2', Value};
field_parse(<<"194=", Value/binary>>) -> 
{'LastSpotRate', to_float(Value)};
field_parse(<<"195=", Value/binary>>) -> 
{'LastForwardPoints', Value};
field_parse(<<"196=", Value/binary>>) -> 
{'AllocLinkID', Value};
field_parse(<<"197=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'F_X_NETTING'; 
<<"1">> -> 'F_X_SWAP'; 
_ -> unknown
end,
{'AllocLinkType', Val};
field_parse(<<"198=", Value/binary>>) -> 
{'SecondaryOrderID', Value};
field_parse(<<"199=", Value/binary>>) -> 
{'NoIOIQualifiers', to_int(Value)};
field_parse(<<"200=", Value/binary>>) -> 
{'MaturityMonthYear', Value};
field_parse(<<"201=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'PUT'; 
<<"1">> -> 'CALL'; 
_ -> unknown
end,
{'PutOrCall', Val};
field_parse(<<"202=", Value/binary>>) -> 
{'StrikePrice', to_float(Value)};
field_parse(<<"203=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'COVERED'; 
<<"1">> -> 'UNCOVERED'; 
_ -> unknown
end,
{'CoveredOrUncovered', Val};
field_parse(<<"204=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'CUSTOMER'; 
<<"1">> -> 'FIRM'; 
_ -> unknown
end,
{'CustomerOrFirm', Val};
field_parse(<<"205=", Value/binary>>) -> 
{'MaturityDay', Value};
field_parse(<<"206=", Value/binary>>) -> 
{'OptAttribute', Value};
field_parse(<<"207=", Value/binary>>) -> 
{'SecurityExchange', Value};
field_parse(<<"208=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'NotifyBrokerOfCredit', Val};
field_parse(<<"209=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'MATCH'; 
<<"2">> -> 'FORWARD'; 
<<"3">> -> 'FORWARD_AND_MATCH'; 
_ -> unknown
end,
{'AllocHandlInst', Val};
field_parse(<<"210=", Value/binary>>) -> 
{'MaxShow', to_float(Value)};
field_parse(<<"211=", Value/binary>>) -> 
{'PegDifference', Value};
field_parse(<<"212=", Value/binary>>) -> 
{'XmlDataLen', to_int(Value)};
field_parse(<<"213=", Value/binary>>) -> 
{'XmlData', Value};
field_parse(<<"214=", Value/binary>>) -> 
{'SettlInstRefID', Value};
field_parse(<<"215=", Value/binary>>) -> 
{'NoRoutingIDs', to_int(Value)};
field_parse(<<"216=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'TARGET_FIRM'; 
<<"2">> -> 'TARGET_LIST'; 
<<"3">> -> 'BLOCK_FIRM'; 
<<"4">> -> 'BLOCK_LIST'; 
_ -> unknown
end,
{'RoutingType', Val};
field_parse(<<"217=", Value/binary>>) -> 
{'RoutingID', Value};
field_parse(<<"218=", Value/binary>>) -> 
{'SpreadToBenchmark', Value};
field_parse(<<"219=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'CURVE'; 
<<"2">> -> '5_YR'; 
<<"3">> -> 'OLD_5'; 
<<"4">> -> '10_YR'; 
<<"5">> -> 'OLD_10'; 
<<"6">> -> '30_YR'; 
<<"7">> -> 'OLD_30'; 
<<"8">> -> '3_MO_LIBOR'; 
<<"9">> -> '6_MO_LIBOR'; 
_ -> unknown
end,
{'Benchmark', Val};
field_parse(<<"223=", Value/binary>>) -> 
{'CouponRate', Value};
field_parse(<<"231=", Value/binary>>) -> 
{'ContractMultiplier', Value};
field_parse(<<"262=", Value/binary>>) -> 
{'MDReqID', Value};
field_parse(<<"263=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'SNAPSHOT'; 
<<"1">> -> 'SNAPSHOT_PLUS_UPDATES'; 
<<"2">> -> 'DISABLE_PREVIOUS_SNAPSHOT_PLUS_UPDATE_REQUEST'; 
_ -> unknown
end,
{'SubscriptionRequestType', Val};
field_parse(<<"264=", Value/binary>>) -> 
{'MarketDepth', to_int(Value)};
field_parse(<<"265=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'FULL_REFRESH'; 
<<"1">> -> 'INCREMENTAL_REFRESH'; 
_ -> unknown
end,
{'MDUpdateType', Val};
field_parse(<<"266=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'AggregatedBook', Val};
field_parse(<<"267=", Value/binary>>) -> 
{'NoMDEntryTypes', to_int(Value)};
field_parse(<<"268=", Value/binary>>) -> 
{'NoMDEntries', to_int(Value)};
field_parse(<<"269=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'BID'; 
<<"1">> -> 'OFFER'; 
<<"2">> -> 'TRADE'; 
<<"3">> -> 'INDEX_VALUE'; 
<<"4">> -> 'OPENING_PRICE'; 
<<"5">> -> 'CLOSING_PRICE'; 
<<"6">> -> 'SETTLEMENT_PRICE'; 
<<"7">> -> 'TRADING_SESSION_HIGH_PRICE'; 
<<"8">> -> 'TRADING_SESSION_LOW_PRICE'; 
<<"9">> -> 'TRADING_SESSION_VWAP_PRICE'; 
_ -> unknown
end,
{'MDEntryType', Val};
field_parse(<<"270=", Value/binary>>) -> 
{'MDEntryPx', to_float(Value)};
field_parse(<<"271=", Value/binary>>) -> 
{'MDEntrySize', to_float(Value)};
field_parse(<<"272=", Value/binary>>) -> 
{'MDEntryDate', Value};
field_parse(<<"273=", Value/binary>>) -> 
{'MDEntryTime', Value};
field_parse(<<"274=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'PLUS_TICK'; 
<<"1">> -> 'ZERO_PLUS_TICK'; 
<<"2">> -> 'MINUS_TICK'; 
<<"3">> -> 'ZERO_MINUS_TICK'; 
_ -> unknown
end,
{'TickDirection', Val};
field_parse(<<"275=", Value/binary>>) -> 
{'MDMkt', Value};
field_parse(<<"276=", Value/binary>>) -> 
Val = case Value of
<<"A">> -> 'OPEN'; 
<<"B">> -> 'CLOSED'; 
<<"C">> -> 'EXCHANGE_BEST'; 
<<"D">> -> 'CONSOLIDATED_BEST'; 
<<"E">> -> 'LOCKED'; 
<<"F">> -> 'CROSSED'; 
<<"G">> -> 'DEPTH'; 
<<"H">> -> 'FAST_TRADING'; 
<<"I">> -> 'NON_FIRM'; 
_ -> unknown
end,
{'QuoteCondition', Val};
field_parse(<<"277=", Value/binary>>) -> 
Val = case Value of
<<"A">> -> 'CASH'; 
<<"B">> -> 'AVERAGE_PRICE_TRADE'; 
<<"C">> -> 'CASH_TRADE'; 
<<"D">> -> 'NEXT_DAY'; 
<<"E">> -> 'OPENING'; 
<<"F">> -> 'INTRADAY_TRADE_DETAIL'; 
<<"G">> -> 'RULE_127_TRADE'; 
<<"H">> -> 'RULE_155_TRADE'; 
<<"I">> -> 'SOLD_LAST'; 
<<"J">> -> 'NEXT_DAY_TRADE'; 
<<"K">> -> 'OPENED'; 
<<"L">> -> 'SELLER'; 
<<"M">> -> 'SOLD'; 
<<"N">> -> 'STOPPED_STOCK'; 
_ -> unknown
end,
{'TradeCondition', Val};
field_parse(<<"278=", Value/binary>>) -> 
{'MDEntryID', Value};
field_parse(<<"279=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NEW'; 
<<"1">> -> 'CHANGE'; 
<<"2">> -> 'DELETE'; 
_ -> unknown
end,
{'MDUpdateAction', Val};
field_parse(<<"280=", Value/binary>>) -> 
{'MDEntryRefID', Value};
field_parse(<<"281=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'UNKNOWN_SYMBOL'; 
<<"1">> -> 'DUPLICATE_MDREQID'; 
<<"2">> -> 'INSUFFICIENT_BANDWIDTH'; 
<<"3">> -> 'INSUFFICIENT_PERMISSIONS'; 
<<"4">> -> 'UNSUPPORTED_SUBSCRIPTIONREQUESTTYPE'; 
<<"5">> -> 'UNSUPPORTED_MARKETDEPTH'; 
<<"6">> -> 'UNSUPPORTED_MDUPDATETYPE'; 
<<"7">> -> 'UNSUPPORTED_AGGREGATEDBOOK'; 
<<"8">> -> 'UNSUPPORTED_MDENTRYTYPE'; 
_ -> unknown
end,
{'MDReqRejReason', Val};
field_parse(<<"282=", Value/binary>>) -> 
{'MDEntryOriginator', Value};
field_parse(<<"283=", Value/binary>>) -> 
{'LocationID', Value};
field_parse(<<"284=", Value/binary>>) -> 
{'DeskID', Value};
field_parse(<<"285=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'CANCELATION'; 
<<"1">> -> 'ERROR'; 
_ -> unknown
end,
{'DeleteReason', Val};
field_parse(<<"286=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'DAILY_OPEN'; 
<<"1">> -> 'SESSION_OPEN'; 
<<"2">> -> 'DELIVERY_SETTLEMENT_PRICE'; 
_ -> unknown
end,
{'OpenCloseSettleFlag', Val};
field_parse(<<"287=", Value/binary>>) -> 
{'SellerDays', to_int(Value)};
field_parse(<<"288=", Value/binary>>) -> 
{'MDEntryBuyer', Value};
field_parse(<<"289=", Value/binary>>) -> 
{'MDEntrySeller', Value};
field_parse(<<"290=", Value/binary>>) -> 
{'MDEntryPositionNo', to_int(Value)};
field_parse(<<"291=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'BANKRUPT'; 
_ -> unknown
end,
{'FinancialStatus', Val};
field_parse(<<"292=", Value/binary>>) -> 
Val = case Value of
<<"A">> -> 'EX_DIVIDEND'; 
<<"B">> -> 'EX_DISTRIBUTION'; 
<<"C">> -> 'EX_RIGHTS'; 
<<"D">> -> 'NEW'; 
<<"E">> -> 'EX_INTEREST'; 
_ -> unknown
end,
{'CorporateAction', Val};
field_parse(<<"293=", Value/binary>>) -> 
{'DefBidSize', to_float(Value)};
field_parse(<<"294=", Value/binary>>) -> 
{'DefOfferSize', to_float(Value)};
field_parse(<<"295=", Value/binary>>) -> 
{'NoQuoteEntries', to_int(Value)};
field_parse(<<"296=", Value/binary>>) -> 
{'NoQuoteSets', to_int(Value)};
field_parse(<<"297=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'ACCEPTED'; 
<<"1">> -> 'CANCELED_FOR_SYMBOL'; 
<<"2">> -> 'CANCELED_FOR_SECURITY_TYPE'; 
<<"3">> -> 'CANCELED_FOR_UNDERLYING'; 
<<"4">> -> 'CANCELED_ALL'; 
<<"5">> -> 'REJECTED'; 
_ -> unknown
end,
{'QuoteAckStatus', Val};
field_parse(<<"298=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'CANCEL_FOR_SYMBOL'; 
<<"2">> -> 'CANCEL_FOR_SECURITY_TYPE'; 
<<"3">> -> 'CANCEL_FOR_UNDERLYING_SYMBOL'; 
<<"4">> -> 'CANCEL_FOR_ALL_QUOTES'; 
_ -> unknown
end,
{'QuoteCancelType', Val};
field_parse(<<"299=", Value/binary>>) -> 
{'QuoteEntryID', Value};
field_parse(<<"300=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'UNKNOWN_SYMBOL'; 
<<"2">> -> 'EXCHANGE'; 
<<"3">> -> 'QUOTE_REQUEST_EXCEEDS_LIMIT'; 
<<"4">> -> 'TOO_LATE_TO_ENTER'; 
<<"5">> -> 'UNKNOWN_QUOTE'; 
<<"6">> -> 'DUPLICATE_QUOTE'; 
<<"7">> -> 'INVALID_BID_ASK_SPREAD'; 
<<"8">> -> 'INVALID_PRICE'; 
<<"9">> -> 'NOT_AUTHORIZED_TO_QUOTE_SECURITY'; 
_ -> unknown
end,
{'QuoteRejectReason', Val};
field_parse(<<"301=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'NO_ACKNOWLEDGEMENT'; 
<<"1">> -> 'ACKNOWLEDGE_ONLY_NEGATIVE_OR_ERRONEOUS_QUOTES'; 
<<"2">> -> 'ACKNOWLEDGE_EACH_QUOTE_MESSAGES'; 
_ -> unknown
end,
{'QuoteResponseLevel', Val};
field_parse(<<"302=", Value/binary>>) -> 
{'QuoteSetID', Value};
field_parse(<<"303=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'MANUAL'; 
<<"2">> -> 'AUTOMATIC'; 
_ -> unknown
end,
{'QuoteRequestType', Val};
field_parse(<<"304=", Value/binary>>) -> 
{'TotQuoteEntries', to_int(Value)};
field_parse(<<"305=", Value/binary>>) -> 
{'UnderlyingIDSource', Value};
field_parse(<<"306=", Value/binary>>) -> 
{'UnderlyingIssuer', Value};
field_parse(<<"307=", Value/binary>>) -> 
{'UnderlyingSecurityDesc', Value};
field_parse(<<"308=", Value/binary>>) -> 
{'UnderlyingSecurityExchange', Value};
field_parse(<<"309=", Value/binary>>) -> 
{'UnderlyingSecurityID', Value};
field_parse(<<"310=", Value/binary>>) -> 
{'UnderlyingSecurityType', Value};
field_parse(<<"311=", Value/binary>>) -> 
{'UnderlyingSymbol', Value};
field_parse(<<"312=", Value/binary>>) -> 
{'UnderlyingSymbolSfx', Value};
field_parse(<<"313=", Value/binary>>) -> 
{'UnderlyingMaturityMonthYear', Value};
field_parse(<<"314=", Value/binary>>) -> 
{'UnderlyingMaturityDay', Value};
field_parse(<<"315=", Value/binary>>) -> 
{'UnderlyingPutOrCall', to_int(Value)};
field_parse(<<"316=", Value/binary>>) -> 
{'UnderlyingStrikePrice', to_float(Value)};
field_parse(<<"317=", Value/binary>>) -> 
{'UnderlyingOptAttribute', Value};
field_parse(<<"318=", Value/binary>>) -> 
{'UnderlyingCurrency', to_float(Value)};
field_parse(<<"319=", Value/binary>>) -> 
{'RatioQty', Value};
field_parse(<<"320=", Value/binary>>) -> 
{'SecurityReqID', Value};
field_parse(<<"321=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'REQUEST_SECURITY_IDENTITY_AND_SPECIFICATIONS'; 
<<"1">> -> 'REQUEST_SECURITY_IDENTITY_FOR_THE_SPECIFICATIONS_PROVIDED'; 
<<"2">> -> 'REQUEST_LIST_SECURITY_TYPES'; 
<<"3">> -> 'REQUEST_LIST_SECURITIES'; 
_ -> unknown
end,
{'SecurityRequestType', Val};
field_parse(<<"322=", Value/binary>>) -> 
{'SecurityResponseID', Value};
field_parse(<<"323=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'ACCEPT_SECURITY_PROPOSAL_AS_IS'; 
<<"2">> -> 'ACCEPT_SECURITY_PROPOSAL_WITH_REVISIONS_AS_INDICATED_IN_THE_MESSAGE'; 
<<"3">> -> 'LIST_OF_SECURITY_TYPES_RETURNED_PER_REQUEST'; 
<<"4">> -> 'LIST_OF_SECURITIES_RETURNED_PER_REQUEST'; 
<<"5">> -> 'REJECT_SECURITY_PROPOSAL'; 
<<"6">> -> 'CAN_NOT_MATCH_SELECTION_CRITERIA'; 
_ -> unknown
end,
{'SecurityResponseType', Val};
field_parse(<<"324=", Value/binary>>) -> 
{'SecurityStatusReqID', Value};
field_parse(<<"325=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'UnsolicitedIndicator', Val};
field_parse(<<"326=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'OPENING_DELAY'; 
<<"10">> -> 'MARKET_ON_CLOSE_IMBALANCE_SELL'; 
<<"11">> -> '11'; 
<<"12">> -> 'NO_MARKET_IMBALANCE'; 
<<"13">> -> 'NO_MARKET_ON_CLOSE_IMBALANCE'; 
<<"14">> -> 'ITS_PRE_OPENING'; 
<<"15">> -> 'NEW_PRICE_INDICATION'; 
<<"16">> -> 'TRADE_DISSEMINATION_TIME'; 
<<"17">> -> 'READY_TO_TRADE'; 
<<"18">> -> 'NOT_AVAILABLE_FOR_TRADING'; 
<<"19">> -> 'NOT_TRADED_ON_THIS_MARKET'; 
<<"2">> -> 'TRADING_HALT'; 
<<"20">> -> 'UNKNOWN_OR_INVALID'; 
<<"3">> -> 'RESUME'; 
<<"4">> -> 'NO_OPEN_NO_RESUME'; 
<<"5">> -> 'PRICE_INDICATION'; 
<<"6">> -> 'TRADING_RANGE_INDICATION'; 
<<"7">> -> 'MARKET_IMBALANCE_BUY'; 
<<"8">> -> 'MARKET_IMBALANCE_SELL'; 
<<"9">> -> 'MARKET_ON_CLOSE_IMBALANCE_BUY'; 
_ -> unknown
end,
{'SecurityTradingStatus', Val};
field_parse(<<"327=", Value/binary>>) -> 
Val = case Value of
<<"D">> -> 'NEWS_DISSEMINATION'; 
<<"E">> -> 'ORDER_INFLUX'; 
<<"I">> -> 'ORDER_IMBALANCE'; 
<<"M">> -> 'ADDITIONAL_INFORMATION'; 
<<"P">> -> 'NEWS_PENDING'; 
<<"X">> -> 'EQUIPMENT_CHANGEOVER'; 
_ -> unknown
end,
{'HaltReasonChar', Val};
field_parse(<<"328=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'InViewOfCommon', Val};
field_parse(<<"329=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'DueToRelated', Val};
field_parse(<<"330=", Value/binary>>) -> 
{'BuyVolume', to_float(Value)};
field_parse(<<"331=", Value/binary>>) -> 
{'SellVolume', to_float(Value)};
field_parse(<<"332=", Value/binary>>) -> 
{'HighPx', to_float(Value)};
field_parse(<<"333=", Value/binary>>) -> 
{'LowPx', to_float(Value)};
field_parse(<<"334=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'CANCEL'; 
<<"2">> -> 'ERROR'; 
<<"3">> -> 'CORRECTION'; 
_ -> unknown
end,
{'Adjustment', Val};
field_parse(<<"335=", Value/binary>>) -> 
{'TradSesReqID', Value};
field_parse(<<"336=", Value/binary>>) -> 
{'TradingSessionID', Value};
field_parse(<<"337=", Value/binary>>) -> 
{'ContraTrader', Value};
field_parse(<<"338=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'ELECTRONIC'; 
<<"2">> -> 'OPEN_OUTCRY'; 
<<"3">> -> 'TWO_PARTY'; 
_ -> unknown
end,
{'TradSesMethod', Val};
field_parse(<<"339=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'TESTING'; 
<<"2">> -> 'SIMULATED'; 
<<"3">> -> 'PRODUCTION'; 
_ -> unknown
end,
{'TradSesMode', Val};
field_parse(<<"340=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'HALTED'; 
<<"2">> -> 'OPEN'; 
<<"3">> -> 'CLOSED'; 
<<"4">> -> 'PRE_OPEN'; 
<<"5">> -> 'PRE_CLOSE'; 
_ -> unknown
end,
{'TradSesStatus', Val};
field_parse(<<"341=", Value/binary>>) -> 
{'TradSesStartTime', Value};
field_parse(<<"342=", Value/binary>>) -> 
{'TradSesOpenTime', Value};
field_parse(<<"343=", Value/binary>>) -> 
{'TradSesPreCloseTime', Value};
field_parse(<<"344=", Value/binary>>) -> 
{'TradSesCloseTime', Value};
field_parse(<<"345=", Value/binary>>) -> 
{'TradSesEndTime', Value};
field_parse(<<"346=", Value/binary>>) -> 
{'NumberOfOrders', to_int(Value)};
field_parse(<<"347=", Value/binary>>) -> 
Val = case Value of
<<"EUC-JP">> -> 'EUC_JP'; 
<<"ISO-2022-JP">> -> 'ISO_2022_JP'; 
<<"SHIFT_JIS">> -> 'SHIFT_JIS'; 
<<"UTF-8">> -> 'UTF_8'; 
_ -> unknown
end,
{'MessageEncoding', Val};
field_parse(<<"348=", Value/binary>>) -> 
{'EncodedIssuerLen', to_int(Value)};
field_parse(<<"349=", Value/binary>>) -> 
{'EncodedIssuer', Value};
field_parse(<<"350=", Value/binary>>) -> 
{'EncodedSecurityDescLen', to_int(Value)};
field_parse(<<"351=", Value/binary>>) -> 
{'EncodedSecurityDesc', Value};
field_parse(<<"352=", Value/binary>>) -> 
{'EncodedListExecInstLen', to_int(Value)};
field_parse(<<"353=", Value/binary>>) -> 
{'EncodedListExecInst', Value};
field_parse(<<"354=", Value/binary>>) -> 
{'EncodedTextLen', to_int(Value)};
field_parse(<<"355=", Value/binary>>) -> 
{'EncodedText', Value};
field_parse(<<"356=", Value/binary>>) -> 
{'EncodedSubjectLen', to_int(Value)};
field_parse(<<"357=", Value/binary>>) -> 
{'EncodedSubject', Value};
field_parse(<<"358=", Value/binary>>) -> 
{'EncodedHeadlineLen', to_int(Value)};
field_parse(<<"359=", Value/binary>>) -> 
{'EncodedHeadline', Value};
field_parse(<<"360=", Value/binary>>) -> 
{'EncodedAllocTextLen', to_int(Value)};
field_parse(<<"361=", Value/binary>>) -> 
{'EncodedAllocText', Value};
field_parse(<<"362=", Value/binary>>) -> 
{'EncodedUnderlyingIssuerLen', to_int(Value)};
field_parse(<<"363=", Value/binary>>) -> 
{'EncodedUnderlyingIssuer', Value};
field_parse(<<"364=", Value/binary>>) -> 
{'EncodedUnderlyingSecurityDescLen', to_int(Value)};
field_parse(<<"365=", Value/binary>>) -> 
{'EncodedUnderlyingSecurityDesc', Value};
field_parse(<<"366=", Value/binary>>) -> 
{'AllocPrice', to_float(Value)};
field_parse(<<"367=", Value/binary>>) -> 
{'QuoteSetValidUntilTime', Value};
field_parse(<<"368=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'UNKNOWN_SYMBOL'; 
<<"2">> -> 'EXCHANGE'; 
<<"3">> -> 'QUOTE_EXCEEDS_LIMIT'; 
<<"4">> -> 'TOO_LATE_TO_ENTER'; 
<<"5">> -> 'UNKNOWN_QUOTE'; 
<<"6">> -> 'DUPLICATE_QUOTE'; 
<<"7">> -> 'INVALID_BID_ASK_SPREAD'; 
<<"8">> -> 'INVALID_PRICE'; 
<<"9">> -> 'NOT_AUTHORIZED_TO_QUOTE_SECURITY'; 
_ -> unknown
end,
{'QuoteEntryRejectReason', Val};
field_parse(<<"369=", Value/binary>>) -> 
{'LastMsgSeqNumProcessed', to_int(Value)};
field_parse(<<"370=", Value/binary>>) -> 
{'OnBehalfOfSendingTime', Value};
field_parse(<<"371=", Value/binary>>) -> 
{'RefTagID', to_int(Value)};
field_parse(<<"372=", Value/binary>>) -> 
{'RefMsgType', Value};
field_parse(<<"373=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'INVALID_TAG_NUMBER'; 
<<"1">> -> 'REQUIRED_TAG_MISSING'; 
<<"10">> -> 'SENDINGTIME_ACCURACY_PROBLEM'; 
<<"11">> -> 'INVALID_MSGTYPE'; 
<<"2">> -> 'TAG_NOT_DEFINED_FOR_THIS_MESSAGE_TYPE'; 
<<"3">> -> 'UNDEFINED_TAG'; 
<<"4">> -> 'TAG_SPECIFIED_WITHOUT_A_VALUE'; 
<<"5">> -> 'VALUE_IS_INCORRECT'; 
<<"6">> -> 'INCORRECT_DATA_FORMAT_FOR_VALUE'; 
<<"7">> -> 'DECRYPTION_PROBLEM'; 
<<"8">> -> 'SIGNATURE_PROBLEM'; 
<<"9">> -> 'COMPID_PROBLEM'; 
_ -> unknown
end,
{'SessionRejectReason', Val};
field_parse(<<"374=", Value/binary>>) -> 
Val = case Value of
<<"C">> -> 'CANCEL'; 
<<"N">> -> 'NO'; 
_ -> unknown
end,
{'BidRequestTransType', Val};
field_parse(<<"375=", Value/binary>>) -> 
{'ContraBroker', Value};
field_parse(<<"376=", Value/binary>>) -> 
{'ComplianceID', Value};
field_parse(<<"377=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'SolicitedFlag', Val};
field_parse(<<"378=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'GT_CORPORATE_ACTION'; 
<<"1">> -> 'GT_RENEWAL'; 
<<"2">> -> 'VERBAL_CHANGE'; 
<<"3">> -> 'REPRICING_OF_ORDER'; 
<<"4">> -> 'BROKER_OPTION'; 
<<"5">> -> 'PARTIAL_DECLINE_OF_ORDERQTY'; 
_ -> unknown
end,
{'ExecRestatementReason', Val};
field_parse(<<"379=", Value/binary>>) -> 
{'BusinessRejectRefID', Value};
field_parse(<<"380=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'OTHER'; 
<<"1">> -> 'UNKOWN_ID'; 
<<"2">> -> 'UNKNOWN_SECURITY'; 
<<"3">> -> 'UNSUPPORTED_MESSAGE_TYPE'; 
<<"4">> -> 'APPLICATION_NOT_AVAILABLE'; 
<<"5">> -> 'CONDITIONALLY_REQUIRED_FIELD_MISSING'; 
_ -> unknown
end,
{'BusinessRejectReason', Val};
field_parse(<<"381=", Value/binary>>) -> 
{'GrossTradeAmt', Value};
field_parse(<<"382=", Value/binary>>) -> 
{'NoContraBrokers', to_int(Value)};
field_parse(<<"383=", Value/binary>>) -> 
{'MaxMessageSize', to_int(Value)};
field_parse(<<"384=", Value/binary>>) -> 
{'NoMsgTypes', to_int(Value)};
field_parse(<<"385=", Value/binary>>) -> 
Val = case Value of
<<"R">> -> 'RECEIVE'; 
<<"S">> -> 'SEND'; 
_ -> unknown
end,
{'MsgDirection', Val};
field_parse(<<"386=", Value/binary>>) -> 
{'NoTradingSessions', to_int(Value)};
field_parse(<<"387=", Value/binary>>) -> 
{'TotalVolumeTraded', to_float(Value)};
field_parse(<<"388=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'RELATED_TO_DISPLAYED_PRICE'; 
<<"1">> -> 'RELATED_TO_MARKET_PRICE'; 
<<"2">> -> 'RELATED_TO_PRIMARY_PRICE'; 
<<"3">> -> 'RELATED_TO_LOCAL_PRIMARY_PRICE'; 
<<"4">> -> 'RELATED_TO_MIDPOINT_PRICE'; 
<<"5">> -> 'RELATED_TO_LAST_TRADE_PRICE'; 
_ -> unknown
end,
{'DiscretionInst', Val};
field_parse(<<"389=", Value/binary>>) -> 
{'DiscretionOffset', Value};
field_parse(<<"390=", Value/binary>>) -> 
{'BidID', Value};
field_parse(<<"391=", Value/binary>>) -> 
{'ClientBidID', Value};
field_parse(<<"392=", Value/binary>>) -> 
{'ListName', Value};
field_parse(<<"393=", Value/binary>>) -> 
{'TotalNumSecurities', to_int(Value)};
field_parse(<<"394=", Value/binary>>) -> 
{'BidType', to_int(Value)};
field_parse(<<"395=", Value/binary>>) -> 
{'NumTickets', to_int(Value)};
field_parse(<<"396=", Value/binary>>) -> 
{'SideValue1', Value};
field_parse(<<"397=", Value/binary>>) -> 
{'SideValue2', Value};
field_parse(<<"398=", Value/binary>>) -> 
{'NoBidDescriptors', to_int(Value)};
field_parse(<<"399=", Value/binary>>) -> 
{'BidDescriptorType', to_int(Value)};
field_parse(<<"400=", Value/binary>>) -> 
{'BidDescriptor', Value};
field_parse(<<"401=", Value/binary>>) -> 
{'SideValueInd', to_int(Value)};
field_parse(<<"402=", Value/binary>>) -> 
{'LiquidityPctLow', Value};
field_parse(<<"403=", Value/binary>>) -> 
{'LiquidityPctHigh', Value};
field_parse(<<"404=", Value/binary>>) -> 
{'LiquidityValue', Value};
field_parse(<<"405=", Value/binary>>) -> 
{'EFPTrackingError', Value};
field_parse(<<"406=", Value/binary>>) -> 
{'FairValue', Value};
field_parse(<<"407=", Value/binary>>) -> 
{'OutsideIndexPct', Value};
field_parse(<<"408=", Value/binary>>) -> 
{'ValueOfFutures', Value};
field_parse(<<"409=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> '5_DAY_MOVING_AVERAGE'; 
<<"2">> -> '20_DAY_MOVING_AVERAGE'; 
<<"3">> -> 'NORMAL_MARKET_SIZE'; 
<<"4">> -> 'OTHER'; 
_ -> unknown
end,
{'LiquidityIndType', Val};
field_parse(<<"410=", Value/binary>>) -> 
{'WtAverageLiquidity', Value};
field_parse(<<"411=", Value/binary>>) -> 
Val = case Value of
<<"N">> -> 'NO'; 
<<"Y">> -> 'YES'; 
_ -> unknown
end,
{'ExchangeForPhysical', Val};
field_parse(<<"412=", Value/binary>>) -> 
{'OutMainCntryUIndex', Value};
field_parse(<<"413=", Value/binary>>) -> 
{'CrossPercent', Value};
field_parse(<<"414=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'BUYSIDE_EXPLICITLY_REQUESTS_STATUS_USING_STATUSREQUEST'; 
<<"2">> -> 'SELLSIDE_PERIODICALLY_SENDS_STATUS_USING_LISTSTATUS_PERIOD_OPTIONALLY_SPECIFIED_IN_PROGRESSPERIOD'; 
<<"3">> -> 'REAL_TIME_EXECUTION_REPORTS'; 
_ -> unknown
end,
{'ProgRptReqs', Val};
field_parse(<<"415=", Value/binary>>) -> 
{'ProgPeriodInterval', to_int(Value)};
field_parse(<<"416=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'NET'; 
<<"2">> -> 'GROSS'; 
_ -> unknown
end,
{'IncTaxInd', Val};
field_parse(<<"417=", Value/binary>>) -> 
{'NumBidders', to_int(Value)};
field_parse(<<"418=", Value/binary>>) -> 
Val = case Value of
<<"A">> -> 'AGENCY'; 
<<"G">> -> 'VWAP_GUARANTEE'; 
<<"J">> -> 'GUARANTEED_CLOSE'; 
<<"R">> -> 'RISK_TRADE'; 
_ -> unknown
end,
{'TradeType', Val};
field_parse(<<"419=", Value/binary>>) -> 
Val = case Value of
<<"2">> -> 'CLOSING_PRICE_AT_MORNING_SESSION'; 
<<"3">> -> 'CLOSING_PRICE'; 
<<"4">> -> 'CURRENT_PRICE'; 
<<"5">> -> 'SQ'; 
<<"6">> -> 'VWAP_THROUGH_A_DAY'; 
<<"7">> -> 'VWAP_THROUGH_A_MORNING_SESSION'; 
<<"8">> -> 'VWAP_THROUGH_AN_AFTERNOON_SESSION'; 
<<"9">> -> 'VWAP_THROUGH_A_DAY_EXCEPT_YORI'; 
<<"A">> -> 'VWAP_THROUGH_A_MORNING_SESSION_EXCEPT_YORI'; 
<<"B">> -> 'VWAP_THROUGH_AN_AFTERNOON_SESSION_EXCEPT_YORI'; 
<<"C">> -> 'STRIKE'; 
<<"D">> -> 'OPEN'; 
<<"Z">> -> 'OTHERS'; 
_ -> unknown
end,
{'BasisPxType', Val};
field_parse(<<"420=", Value/binary>>) -> 
{'NoBidComponents', to_int(Value)};
field_parse(<<"421=", Value/binary>>) -> 
{'Country', Value};
field_parse(<<"422=", Value/binary>>) -> 
{'TotNoStrikes', to_int(Value)};
field_parse(<<"423=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'PERCENTAGE'; 
<<"2">> -> 'PER_SHARE'; 
<<"3">> -> 'FIXED_AMOUNT'; 
_ -> unknown
end,
{'PriceType', Val};
field_parse(<<"424=", Value/binary>>) -> 
{'DayOrderQty', to_float(Value)};
field_parse(<<"425=", Value/binary>>) -> 
{'DayCumQty', to_float(Value)};
field_parse(<<"426=", Value/binary>>) -> 
{'DayAvgPx', to_float(Value)};
field_parse(<<"427=", Value/binary>>) -> 
Val = case Value of
<<"0">> -> 'BOOK_OUT_ALL_TRADES_ON_DAY_OF_EXECUTION'; 
<<"1">> -> 'ACCUMULATE_EXECUTIONS_UNTIL_ORDER_IS_FILLED_OR_EXPIRES'; 
<<"2">> -> 'ACCUMULATE_UNTIL_VERBALLY_NOTIFIED_OTHERWISE'; 
_ -> unknown
end,
{'GTBookingInst', Val};
field_parse(<<"428=", Value/binary>>) -> 
{'NoStrikes', to_int(Value)};
field_parse(<<"429=", Value/binary>>) -> 
{'ListStatusType', to_int(Value)};
field_parse(<<"430=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'NET'; 
<<"2">> -> 'GROSS'; 
_ -> unknown
end,
{'NetGrossInd', Val};
field_parse(<<"431=", Value/binary>>) -> 
{'ListOrderStatus', to_int(Value)};
field_parse(<<"432=", Value/binary>>) -> 
{'ExpireDate', Value};
field_parse(<<"433=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'IMMEDIATE'; 
<<"2">> -> 'WAIT_FOR_EXECUTE_INSTRUCTION'; 
_ -> unknown
end,
{'ListExecInstType', Val};
field_parse(<<"434=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'ORDER_CANCEL_REQUEST'; 
<<"2">> -> 'ORDER_CANCEL_REPLACE_REQUEST'; 
_ -> unknown
end,
{'CxlRejResponseTo', Val};
field_parse(<<"435=", Value/binary>>) -> 
{'UnderlyingCouponRate', Value};
field_parse(<<"436=", Value/binary>>) -> 
{'UnderlyingContractMultiplier', Value};
field_parse(<<"437=", Value/binary>>) -> 
{'ContraTradeQty', to_float(Value)};
field_parse(<<"438=", Value/binary>>) -> 
{'ContraTradeTime', Value};
field_parse(<<"439=", Value/binary>>) -> 
{'ClearingFirm', Value};
field_parse(<<"440=", Value/binary>>) -> 
{'ClearingAccount', Value};
field_parse(<<"441=", Value/binary>>) -> 
{'LiquidityNumSecurities', to_int(Value)};
field_parse(<<"442=", Value/binary>>) -> 
Val = case Value of
<<"1">> -> 'SINGLE_SECURITY'; 
<<"2">> -> 'INDIVIDUAL_LEG_OF_A_MULTI_LEG_SECURITY'; 
<<"3">> -> 'MULTI_LEG_SECURITY'; 
_ -> unknown
end,
{'MultiLegReportingType', Val};
field_parse(<<"443=", Value/binary>>) -> 
{'StrikeTime', Value};
field_parse(<<"444=", Value/binary>>) -> 
{'ListStatusText', Value};
field_parse(<<"445=", Value/binary>>) -> 
{'EncodedListStatusTextLen', to_int(Value)};
field_parse(<<"446=", Value/binary>>) -> 
{'EncodedListStatusText', Value};


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

to_int(Binary) ->
    list_to_integer(binary_to_list(Binary)).

to_float(Binary) -> Binary.
    %list_to_float(binary_to_list(Binary)).
