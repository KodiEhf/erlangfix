-define(SOH, 16#01).
-define(BeginString, 8).

-record('Header', {'DeliverToSubID','DeliverToCompID','OrigSendingTime','OnBehalfOfSubID','OnBehalfOfCompID','PossResend','TargetSubID','TargetCompID','SendingTime','SenderSubID','SenderCompID','PossDupFlag','MsgSeqNum','MsgType','BodyLength','BeginString'}).
-record('Heartbeat', {'TestReqID'}).
-record('Logon', {'ResetSeqNumFlag','HeartBtInt','EncryptMethod'}).
-record('TestRequest', {'TestReqID'}).
-record('ResendRequest', {'EndSeqNo','BeginSeqNo'}).
-record('Reject', {'SessionRejectReason','RefMsgType','RefTagID','Text','RefSeqNum'}).
-record('SequenceReset', {'GapFillFlag','NewSeqNo'}).
-record('Logout', {'Text'}).
-record('NewOrderSingle', {'BrSeqNbr','CrossTradeFlag','DisplayInst','ClRefID','SubMktID','OrderRestrictions','OrderCapacity','ClearingAccount','ClearingFirm','PegDifference','ExpireTime','MaxFloor','MinQty','ExecBroker','TransactTime','TimeInForce','Symbol','Side','SecurityID','Price','OrdType','OrderQty','HandlInst','ExecInst','Currency','ClOrdID'}).
-record('ExecutionReport', {'LockedIn','TradingsessionSubId','TradeLinkId','SecondaryTradeReportRefID','TradeReportRefId','TimestampCounterpart','TimestampOwn','DisseminationTime','LockedInStatus','BreakIndicator','PriceType','StrikeTime','TradeReportRejectReason','ContraClearingAcct','ContraTradePA','DelayedDissemination','OverrideFlag','ClearingPrice','MICCode','RFQReferenceNo','ContraOrderRestrictions','Memo','TrdRptStatus','TradeReportType','PublishTrdIndicator','TrdType','ReversalIndicator','ClearingInstruction','TradeReportID','LiquidityFlag','BrSeqNbr','CrossTradeFlag','DisplayInst','ClRefID','SubMktID','TradeId','TrdMatchId','OrderRestrictions','OrderCapacity','TransBkdTime','ClearingAccount','ClearingFirm','NoContraBrokers','ContraBroker','SettlBrkrCode','LeavesQty','ExecType','ExpireTime','PegDifference','MaxFloor','MinQty','ClientID','ExecBroker','SecurityDesc','ProcessCode','FutSettDate','TradeDate','TransactTime','TimeInForce','ExecRestatementReason','OrdRejReason','Text','Symbol','Side','SecurityID','Price','OrigClOrdID','OrdType','OrdStatus','OrderQty','SecondaryOrderID','OrderID','LastShares','LastPx','ExecTransType','ExecRefID','ExecInst','ExecID','Currency','CumQty','ClOrdID','AvgPx'}).
-record('OrderCancelReplaceRequest', {'ClRefID','BrSeqNbr','SubMktID','ClearingAccount','ClearingFirm','ExpireTime','MaxFloor','TransactTime','TimeInForce','Text','Symbol','Side','SecurityID','Price','OrigClOrdID','OrdType','OrderQty','OrderID','HandlInst','Currency','ClOrdID'}).
-record('OrderCancelRequest', {'SubMktID','TransactTime','Symbol','Side','SecurityID','OrigClOrdID','OrderQty','OrderID','Currency','ClOrdID'}).
-record('OrderCancelReject', {'CxlRejResponseTo','ClientID','CxlRejReason','Text','OrigClOrdID','OrdStatus','OrderID','ClOrdID'}).
-record('Trailer', {'CheckSum'}).

-record(header, {'BeginString', 'BodyLength', 'MsgType', 
		 'MsgSeqNum', 'PossDupFlag', 'SenderCompID',
		 'SenderSubID', 'SendingTime', 'TargetCompID',
		 'TargetSubID', 'PossResend', 'OnBehalfOfCompID',
		 'OnBehalfOfSubID', 'OrigSendingTime'}).

-record(trailer, {'CheckSum' }).

-record(fix_message, {msg_type, fields=[]}).
