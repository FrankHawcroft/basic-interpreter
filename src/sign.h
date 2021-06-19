/****** sign.h ******/

/*
	$VER: sign.h 0.16A (5.5.2015)
*/

#ifndef SIGN_H_INCLUDED
#define SIGN_H_INCLUDED

INLINE int Sign(long value) { return (value > 0) - (value < 0); }

INLINE int FSign(double x) { return (x > 0.0) - (x < 0.0); }

#endif
