-define(SOH, 16#01).
-define(BeginString, 8).

-record(header, {'BeginString', 'BodyLength', 'MsgType', 
		 'MsgSeqNum', 'PossDupFlag', 'SenderCompID',
		 'SenderSubID', 'SendingTime', 'TargetCompID',
		 'TargetSubID', 'PossResend', 'OnBehalfOfCompID',
		 'OnBehalfOfSubID', 'OrigSendingTime'}).

-record(trailer, {'CheckSum' }).

-record(fix_message, {msg_type, fields=[]}).
