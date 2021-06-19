'' Event handling tests.
'' TODO should test SUSPEND, DISABLE, and ON GOTO

sequence% = 0

sub TimerHandler
	shared sequence
	
	print "Timer handler called:", sequence, "at timer = ", timer
	sequence = sequence + 1
	
	if sequence = 2 then
		break '' will fire break handler since it's higher priority
	endif
endsub

sub BreakHandler
	shared sequence
	
	print "Break handler called:", sequence
	sequence = sequence + 1
	
	ErrorLabel:
	
	if sequence > 4 then
		'xstack
		resume recover
	endif
	
	error 234
endsub

sub ErrorHandler
	shared sequence

	print "Error handler called:", sequence, "ERR:", err, "ERLAB: ", erlab, "ERL: ", erl
	
	if err <> 234 or erlab <> "ErrorLabel"
		print "error: failed test: ERR not expected value of 234 and/or ERLAB not 'ErrorLabel'"
		system 1
	endif
	
	sequence = sequence + 1
	
	break '' should have no effect immediately but than cause break handler once sub exits
	print "Break event was correctly deferred"
endsub

on timer(1) call timerhandler
on break call breakhandler
on error call errorhandler

enable timer
' BREAK and ERROR are enabled by default.

repeat
	sleep
forever

recover:
print "Successful RESUME"
