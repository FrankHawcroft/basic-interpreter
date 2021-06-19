
def AmountInWords$(Amount&) where Amount = 0 as "No dollars and no cents"
def AmountInWords$(Amount&) where Amount > 0 as _
	"Exactly " + DollarAndCentString(Amount \ 100, Amount mod 100) + "only"

def DollarAndCentString$(Dollars&, Cents&) where Dollars = 0 and Cents = 0 as _
	""
def DollarAndCentString$(Dollars&, Cents&) where Dollars = 0 and Cents = 1 as _
	"one cent "
def DollarAndCentString$(Dollars&, Cents&) where Dollars = 1 and Cents = 0 as _
	"one dollar "
def DollarAndCentString$(Dollars&, Cents&) where Dollars = 0 as _
	EnglishNumber(Cents) + "cents "
def DollarAndCentString$(Dollars&, Cents&) where Cents = 0 as _
	EnglishNumber(Dollars) + "dollars "
def DollarAndCentString$(Dollars&, Cents&) as DollarAndCentString(Dollars, 0) _
	+ "and " + DollarAndCentString(0, Cents)

def EnglishNumber$(n&) where n >= 1000000 as _
	EnglishNumber(n \ 1000000) + "million " + EnglishNumber(n mod 1000000)
def EnglishNumber$(n&) where n >= 1000 as _
	EnglishNumber(n \ 1000) + "thousand " + EnglishNumber(n mod 1000)
def EnglishNumber$(n&) where n >= 100 and n mod 100 <> 0 as _
	EnglishNumber(n \ 100) + "hundred and " + EnglishNumber(n mod 100)
def EnglishNumber$(n&) where n >= 100 as _
	EnglishNumber(n \ 100) + "hundred "
def EnglishNumber$(n&) where n >= 20 and n mod 10 <> 0 as _
  left(EnglishNumber(n - n mod 10), len(EnglishNumber(n - n mod 10)) - 1) _
  + "-" + EnglishNumber(n mod 10)
def EnglishNumber$(n&) where n = 90 as "ninety "
def EnglishNumber$(n&) where n = 80 as "eighty "
def EnglishNumber$(n&) where n = 70 as "seventy "
def EnglishNumber$(n&) where n = 60 as "sixty "
def EnglishNumber$(n&) where n = 50 as "fifty "
def EnglishNumber$(n&) where n = 40 as "forty "
def EnglishNumber$(n&) where n = 30 as "thirty "
def EnglishNumber$(n&) where n = 20 as "twenty "
def EnglishNumber$(n&) where n = 19 as "nineteen "
def EnglishNumber$(n&) where n = 18 as "eighteen "
def EnglishNumber$(n&) where n = 17 as "seventeen "
def EnglishNumber$(n&) where n = 16 as "sixteen "
def EnglishNumber$(n&) where n = 15 as "fifteen "
def EnglishNumber$(n&) where n = 14 as "fourteen "
def EnglishNumber$(n&) where n = 13 as "thirteen "
def EnglishNumber$(n&) where n = 12 as "twelve "
def EnglishNumber$(n&) where n = 11 as "eleven "
def EnglishNumber$(n&) where n = 10 as "ten "
def EnglishNumber$(n&) where n = 9 as "nine "
def EnglishNumber$(n&) where n = 8 as "eight "
def EnglishNumber$(n&) where n = 7 as "seven "
def EnglishNumber$(n&) where n = 6 as "six "
def EnglishNumber$(n&) where n = 5 as "five "
def EnglishNumber$(n&) where n = 4 as "four "
def EnglishNumber$(n&) where n = 3 as "three "
def EnglishNumber$(n&) where n = 2 as "two "
def EnglishNumber$(n&) where n = 1 as "one "
def EnglishNumber$(n&) where n = 0 as ""

optionbase 1
const nTests% = 8
dim Amt&(nTests)
dim Expected$(nTests)
for I = 1 to nTests | read Amt(I), Expected(I) | next
data 0, "No dollars and no cents"
data 1, "Exactly one cent only"
data 10000, "Exactly one hundred dollars only"
data 9999, "Exactly ninety-nine dollars and ninety-nine cents only"
data 32767, "Exactly three hundred and twenty-seven dollars and sixty-seven cents only"
data 400, "Exactly four dollars only"
data 1500000, "Exactly fifteen thousand dollars only"
data 123456, "Exactly one thousand two hundred and thirty-four dollars and fifty-six cents only"

for I = 1 to nTests
	inWords$ = AmountInWords(Amt(I))
	if inWords = Expected(I)
		print inWords
	else
		print inWords; " <> ", Expected(I)
		error 234
	endif
next
'XFREE

'Amt$ = ""
'lineinput ,Amt
'while Amt <> "-"
'	print AmountInWords(val(Amt))
'	lineinput ,Amt
'wend
