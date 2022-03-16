/****** builtin.h ******/

/*
	$VER: builtin.h 0.16A (9.10.2015)

	Built-in BASIC commands, control-flow statements, and functions.
*/

#ifndef BAS_BUILTIN_H_INCLUDED
#define BAS_BUILTIN_H_INCLUDED

/*** Commands and statements ***/

extern void EmptyStatement_(const QString *, unsigned);
extern void Area_(BObject *, unsigned);
extern void AreaFill_(BObject *, unsigned);
extern void AreaStep_(BObject *, unsigned);
extern void Beep_(BObject *, unsigned);
extern void Break_(BObject *, unsigned);
extern void Case_(BObject *, unsigned);
extern void Chain_(BObject *, unsigned); /* TODO */
extern void ChDir_(BObject *, unsigned);
extern void Circle_(BObject *, unsigned);
extern void CircleStep_(BObject *, unsigned);
extern void Clear_(BObject *, unsigned);
extern void Close_(BObject *, unsigned);
extern void Colour_(BObject *, unsigned);
extern void Common_(const QString *, unsigned); /* TODO */
extern void Cont_(BObject *, unsigned); /* TODO */
extern void Data_(const QString *, unsigned);
extern void Def_(const QString *, unsigned);
extern void DefBln_(const QString *, unsigned);
extern void DefChr_(const QString *, unsigned);
extern void DefDbl_(const QString *, unsigned);
extern void DefInt_(const QString *, unsigned);
extern void DefLng_(const QString *, unsigned);
extern void DefSng_(const QString *, unsigned);
extern void DefStr_(const QString *, unsigned);
extern void Dim_(BObject *, unsigned);
extern void Disable_(const QString *, unsigned);
extern void Else_(BObject *, unsigned);
extern void ElseIf_(BObject *, unsigned);
extern void Enable_(const QString *, unsigned);
extern void End_(BObject *, unsigned);
extern void EndIf_(BObject *, unsigned);
extern void EndSelect_(BObject *, unsigned);
extern void EndSub_(BObject *, unsigned);
extern void Erase_(BObject *, unsigned);
extern void Error_(BObject *, unsigned);
extern void ExitSub_(BObject *, unsigned);
extern void Field_(BObject *, unsigned);
extern void Files_(BObject *, unsigned);
extern void FInput_(BObject *, unsigned);
extern void FLineInput_(BObject *, unsigned);
extern void For_(BObject *, unsigned);
extern void Forever_(BObject *, unsigned);
extern void Forget_(const QString *, unsigned);
extern void FPrint_(BObject *, unsigned);
extern void Get_(BObject *, unsigned);
extern void GoSub_(BObject *, unsigned);
extern void GoTo_(BObject *, unsigned);
extern void If_(BObject *, unsigned);
extern void IfGoTo_(BObject *, unsigned);
extern void IfThenElse_(const QString *, unsigned);
extern void IfThenLet_(BObject *, unsigned);
extern void Input_(BObject *, unsigned);
extern void Kill_(BObject *, unsigned);
extern void Let_(BObject *, unsigned);
extern void Line_(BObject *, unsigned); /* TODO box options */
extern void LineTo_(BObject *, unsigned); /* TODO should be LINE STEP? */
extern void LineInput_(BObject *, unsigned);
extern void Load_(BObject *, unsigned);
/*extern void Locate_(BObject *, unsigned);*/ /* TODO */
extern void Menu_(BObject *, unsigned);
extern void Merge_(BObject *, unsigned);
extern void Name_(BObject *, unsigned);
extern void Next_(BObject *, unsigned);
extern void NextVar_(BObject *, unsigned);
extern void On_(const QString *, unsigned);
extern void OnGoSub_(BObject *, unsigned);
extern void OnGoTo_(BObject *, unsigned);
extern void Open_(BObject *, unsigned);
extern void OptionBase_(BObject *, unsigned);
extern void Otherwise_(BObject *, unsigned);
extern void Paint_(BObject *, unsigned);
extern void Palette_(BObject *, unsigned);
extern void Pattern_(BObject *, unsigned);
extern void Poke_(BObject *, unsigned);
extern void PokeL_(BObject *, unsigned);
extern void PokeW_(BObject *, unsigned);
extern void PReset_(BObject *, unsigned);
extern void Print_(BObject *, unsigned);
extern void PrintUsing_(BObject *, unsigned); /* TODO */
extern void PSet_(BObject *, unsigned);
extern void Put_(BObject *, unsigned);
extern void Randomize_(BObject *, unsigned);
extern void Read_(BObject *, unsigned);
extern void Repeat_(BObject *, unsigned);
extern void Restore_(const QString *, unsigned);
extern void Resume_(const QString *, unsigned);
extern void Return_(BObject *, unsigned);
extern void ReturnTo_(BObject *, unsigned);
extern void Run_(BObject *, unsigned);
extern void Screen_(BObject *, unsigned);
extern void ScreenClose_(BObject *, unsigned);
extern void ScreenGet_(BObject *, unsigned);
extern void ScreenPut_(BObject *, unsigned);
extern void Scroll_(BObject *, unsigned);
extern void Select_(BObject *, unsigned);
extern void Shared_(const QString *, unsigned);
extern void Sleep_(BObject *, unsigned);
extern void Sound_(BObject *, unsigned);
extern void SoundResume_(BObject *, unsigned);
extern void SoundWait_(BObject *, unsigned);
extern void Stop_(BObject *, unsigned);
extern void Sub_(const QString *, unsigned);
extern void Suspend_(const QString *, unsigned);
extern void Swap_(BObject *, unsigned);
extern void System_(BObject *, unsigned);
/*extern void Tab_(BObject *, unsigned);*/ /* TODO */
extern void TrOff_(BObject *, unsigned);
extern void TrOn_(BObject *, unsigned);
extern void Until_(BObject *, unsigned);
extern void Wait_(BObject *, unsigned);
extern void Wave_(BObject *, unsigned);
extern void WaveSin_(BObject *, unsigned);
extern void WClS_(BObject *, unsigned);
extern void WEnd_(BObject *, unsigned);
extern void While_(BObject *, unsigned);
extern void Window_(BObject *, unsigned);
extern void WindowActivate_(BObject *, unsigned); /* TODO */
extern void WindowClose_(BObject *, unsigned);
extern void WindowOutput_(BObject *, unsigned);
extern void WindowToFront_(BObject *, unsigned); /* TODO */
extern void Write_(BObject *, unsigned);
extern void WLocate_(BObject *, unsigned);
extern void WPrint_(BObject *, unsigned);
extern void WTab_(BObject *, unsigned); /* TODO */

#ifdef DEBUG
extern void XCache_(BObject *, unsigned);
extern void XDoc_(BObject *, unsigned);
extern void XFree_(BObject *, unsigned);
extern void XObj_(const QString *, unsigned);
extern void XStack_(BObject *, unsigned);
#endif

/*** Functions ***/

extern void YieldScalarValue_(Scalar *, const BObject *, unsigned);
extern void ArgC_(Scalar *, const BObject *, unsigned);
extern void ArgV_(Scalar *, const BObject *, unsigned);
extern void Atn_(Scalar *, const BObject *, unsigned);
extern void CallByName_(Scalar *, const BObject *, unsigned);
extern void Collision_(Scalar *, const BObject *, unsigned);
extern void Cos_(Scalar *, const BObject *, unsigned);
extern void Cvb_(Scalar *, const BObject *, unsigned);
extern void Cvd_(Scalar *, const BObject *, unsigned);
extern void Cvi_(Scalar *, const BObject *, unsigned);
extern void Cvl_(Scalar *, const BObject *, unsigned);
extern void Cvs_(Scalar *, const BObject *, unsigned);
extern void Date_(Scalar *, const BObject *, unsigned);
extern void Eof_(Scalar *, const BObject *, unsigned);
extern void Erl_(Scalar *, const BObject *, unsigned);
extern void ErLab_(Scalar *, const BObject *, unsigned);
extern void Err_(Scalar *, const BObject *, unsigned);
extern void Executed_(Scalar *, const BObject *, unsigned);
extern void Exp_(Scalar *, const BObject *, unsigned);
extern void Fre_(Scalar *, const BObject *, unsigned);
extern void FRead_(Scalar *, const BObject *, unsigned);
extern void InKey_(Scalar *, const BObject *, unsigned);
extern void InStr_(Scalar *, const BObject *, unsigned);
extern void LBound_(Scalar *, const BObject *, unsigned);
extern void Len_(Scalar *, const BObject *, unsigned);
extern void Loc_(Scalar *, const BObject *, unsigned);
extern void Lof_(Scalar *, const BObject *, unsigned);
extern void Log_(Scalar *, const BObject *, unsigned);
extern void MenuPicked_(Scalar *, const BObject *, unsigned);
extern void MenuState_(Scalar *, const BObject *, unsigned);
extern void Mid_(Scalar *, const BObject *, unsigned);
extern void Mkb_(Scalar *, const BObject *, unsigned);
extern void Mkd_(Scalar *, const BObject *, unsigned);
extern void Mki_(Scalar *, const BObject *, unsigned);
extern void Mkl_(Scalar *, const BObject *, unsigned);
extern void Mks_(Scalar *, const BObject *, unsigned);
extern void Mouse_(Scalar *, const BObject *, unsigned);
extern void ObjectVX_(Scalar *, const BObject *, unsigned);
extern void ObjectVY_(Scalar *, const BObject *, unsigned);
extern void ObjectX_(Scalar *, const BObject *, unsigned);
extern void ObjectY_(Scalar *, const BObject *, unsigned);
extern void Peek_(Scalar *, const BObject *, unsigned);
extern void PeekL_(Scalar *, const BObject *, unsigned);
extern void PeekW_(Scalar *, const BObject *, unsigned);
extern void Point_(Scalar *, const BObject *, unsigned);
extern void Random_(Scalar *, const BObject *, unsigned);
extern void Rnd_(Scalar *, const BObject *, unsigned);
extern void SAdd_(Scalar *, const BObject *, unsigned);
extern void ScreenInfo_(Scalar *, const BObject *, unsigned); /* TODO */
extern void Sin_(Scalar *, const BObject *, unsigned);
extern void Sqr_(Scalar *, const BObject *, unsigned);
extern void Status_(Scalar *, const BObject *, unsigned);
extern void Str_(Scalar *, const BObject *, unsigned);
extern void Tan_(Scalar *, const BObject *, unsigned);
extern void Time_(Scalar *, const BObject *, unsigned);
extern void Timer_(Scalar *, const BObject *, unsigned);
extern void UBound_(Scalar *, const BObject *, unsigned);
extern void Val_(Scalar *, const BObject *, unsigned);
extern void VarPtr_(Scalar *, const BObject *, unsigned);
extern void WCsrLin_(Scalar *, const BObject *, unsigned);
extern void WindowInfo_(Scalar *, const BObject *, unsigned);
extern void WPos_(Scalar *, const BObject *, unsigned);
extern void WPTab_(Scalar *, const BObject *, unsigned);

/*** Token-->BObject converters, aka 'interners' ***/

extern void DefaultConvert(unsigned, const QString *, BObject *);
extern void EmptyConvert(unsigned, const QString *, BObject *);
extern void ConstConvert(unsigned, const QString *, BObject *);
extern void DimConvert(unsigned, const QString *, BObject *);
extern void DimSharedConvert(unsigned, const QString *, BObject *);
extern void JumpConvert(unsigned, const QString *, BObject *);
extern void ConditionalJumpConvert(unsigned, const QString *, BObject *);
extern void AssignConvert(unsigned, const QString *, BObject *);
extern void LocalScalarAssignConvert(unsigned, const QString *, BObject *);
/* There's also FunctionExpressionConvert, which is only used in functions.c */

/*** Control-flow stack management for non-taken branches ***/

extern bool DefaultInactive(struct Process *, bool);
extern bool EmptyInactive(struct Process *, bool);
extern bool DataInactive(struct Process *, bool);
extern bool IfInactive(struct Process *, bool);
extern bool ForInactive(struct Process *, bool);
extern bool WhileInactive(struct Process *, bool);
extern bool RepeatInactive(struct Process *, bool);
extern bool SelectInactive(struct Process *, bool);
extern bool EndIfInactive(struct Process *, bool);
extern bool WEndInactive(struct Process *, bool);
extern bool NextInactive(struct Process *, bool);
extern bool UntilInactive(struct Process *, bool);
extern bool EndSelectInactive(struct Process *, bool);
extern bool ElseInactive(struct Process *, bool);
extern bool CaseInactive(struct Process *, bool);
extern bool EndSubInactive(struct Process *, bool);
extern bool SubprogramOnlyInactive(struct Process *, bool);

#endif /* BAS_BUILTIN_H_INCLUDED */
