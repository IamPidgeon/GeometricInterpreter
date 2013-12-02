(* University of Washington, Programming Languages, Homework 7
   hw7testsprovided.sml *)
(* Will not compile until you implement preprocess and eval_prog *)

(* These tests do NOT cover all the various cases, especially for intersection *)

use "hw7.sml";

(* Must implement preprocess_prog and Shift before running these tests *)

fun real_equal(x,y) = Real.compare(x,y) = General.EQUAL;

(* a check_equal helper function would be useful here *)

(* Preprocess tests *)
let
	val Point(a,b) = preprocess_prog(LineSegment(3.2,4.1,3.2,4.1))
	val Point(c,d) = Point(3.2,4.1)
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "1-1: PASS: preprocess converts a LineSegment to a Point successfully\n")
	else (print "1-1: FAIL: preprocess does not convert a LineSegment to a Point succesfully\n")
end;

let val LS = LineSegment(3.2,4.1,3.2,4.2)
in 
    let
	val LineSegment(a,b,c,d) = preprocess_prog(LS)
	val LineSegment(e,f,g,h) = LS
    in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "1-2: PASS: preprocess returns proper line segment\n")
	else (print "1-2: FAIL: preprocess does not return proper line segment\n")
    end
end;

let
	val Point(a,b) = preprocess_prog(LineSegment(3.2,4.00001,3.2,4.000005))
	val Point(c,d) = Point(3.2,4.00001)
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "1-3: PASS: real_close_point true works fine in pre-process\n")
	else (print "1-3: FAIL: real_close_point not evaluating points < epsilon correctly in preprocess\n")
end;

let val LT =  LineSegment(3.2,4.0001,3.2,4.0002)
in
    let
	val LineSegment(a,b,c,d) = preprocess_prog LT
	val LineSegment(e,f,g,h) = LT
    in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "1-4: PASS: real_close_point false works fine in pre-process\n")
	else (print "1-4: FAIL: real_close_point not evaluating points > epsilon correctly in preprocess\n")
    end
end;

let val Pt = Point(3.0,5.0)
in
    let
	val Point(a,b) = preprocess_prog Pt
	val Point(c,d) = Pt
    in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "1-5: PASS: pre-process returns input point unchanged\n")
	else (print "1-5: FAIL: pre-process does not return input point unchanged\n")
    end
end;

let val VL = VerticalLine 3.0
in
    let
	val VerticalLine(a)= preprocess_prog VL
	val VerticalLine(b) = VL
    in
	if real_equal(a,b)
	then (print "1-6: PASS: pre-process returns VerticalLine unchanged\n")
	else (print "1-6: FAIL: pre-process does not return VerticalLine unchanged\n")
    end
end;

let
	val LineSegment(a,b,c,d) = preprocess_prog (Let("seg", LineSegment(3.2,4.0001,3.2,4.0002), Var("seg")))
	val LineSegment(e,f,g,h) = LineSegment(3.2,4.0001,3.2,4.0002)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "1-7: PASS: let works correctly in pre-process\n")
	else (print "1-7: FAIL: let works incorrectly in pre-process\n")
end;

(* should write tests for all other constructors in geom_exp for production use *)

let
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(3.2,~4.1,~3.2,4.1))
	val LineSegment(e,f,g,h) = LineSegment(~3.2,4.1,3.2,~4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "1-8: PASS: preprocess flips an improper LineSegment successfully from x\n")
	else (print "1-8: FAIL: preprocess does not flip an improper LineSegment successfully from x\n")
end;

let
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(3.2,4.1,~3.2,~4.1))
	val LineSegment(e,f,g,h) = LineSegment(~3.2,~4.1,3.2,4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "1-9: PASS: preprocess flips an improper LineSegment successfully (x and y)\n")
	else (print "1-9: FAIL: preprocess does not flip an improper LineSegment successfully (x and y)\n")
end;

let
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(~3.2,4.1,~3.2,~4.1))
	val LineSegment(e,f,g,h) = LineSegment(~3.2,~4.1,~3.2,4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "1-10: PASS: preprocess flips an improper LineSegment successfully from y\n")
	else (print "1-10: FAIL: preprocess does not flip an improper LineSegment successfully from y\n")
end;

(* eval_prog tests with Shift*)
let 
	val Point(a,b) = (eval_prog (preprocess_prog (Shift(3.0, 4.0, Point(4.0,4.0))), []))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "2-1: PASS: eval_prog with empty environment worked\n")
	else (print "2-1: FAIL: eval_prog with empty environment is not working properly\n")
end;


(* Using a Var *)
let 
	val Point(a,b) = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0))]))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "2-2: PASS: eval_prog with 'a' in environment is working properly\n")
	else (print "2-2: FAIL: eval_prog with 'a' in environment is not working properly\n")
end;

(* Using a Var *)
let 
	val Point(a,b) = (eval_prog (Shift(~5.0,4.0,Var "a"), [("a",Point(4.0,4.0))]))
	val Point(c,d) = Point(~1.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "2-3: PASS: eval_prog with negative shift is working properly\n")
	else (print "2-3: FAIL: eval_prog with negative shift is not working properly\n")
end;


(* With Variable Shadowing *)
let 
	val Point(a,b) = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0)),("a",Point(1.0,1.0))]))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "2-3: PASS: eval_prog with shadowing 'a' in environment is working properly\n")
	else (print "2-13 FAIL: eval_prog with shadowing 'a' in environment is not working properly\n")
end;
