class Main inherits IO {

   stack : List <- new List;
   copy : List <- new List;
   input : String;
   head : Int <- 0;
   sum : Int <- 0;
   sumStr : String <- "";
   temp : String <- "";
   temp2 : String <- "";
   count : Int <- 0;
   init : Bool <- false;
   inputConv : Int;

   atoi(str : String) : Int {
      let convertor : A2I <- new A2I in {
        convertor.a2i(str);
      }
   };

   exitLoop() : Bool {
      if stack.isNil() then false
      else if stack.head() = "x"
      then true
      else false
      fi fi
   };

   initCheck() : Object {
      if not init then {
         stack <- new List;
         init <- true;
      }
      else
         0
      fi
   };

   itoa(int : Int) : String {
      let convertor : A2I <- new A2I in {
        convertor.i2a(int);
      }
   };

   evaluate() : Object {
      if stack.head() = "e"
         then {
            stack <- stack.tail();
            if (not stack.isNil()) then {
               if (stack.head() = "+")
                  then {
                     while count<2 loop {
                           stack <- stack.tail();
                           head <- atoi(stack.head());
                           sum <- sum + head;
                           count <- count + 1; 
                     }
                     pool;
                     stack <- stack.tail();
                     sumStr <- itoa(sum);
                     stack <- stack.cons(sumStr);
                     count <- 0;
                     sum <- 0;
                  }
               else if (stack.head() = "s")
                  then {
                     stack <- stack.tail();
                     temp <- stack.head();
                     stack <- stack.tail();
                     temp2 <- stack.head();
                     stack <- stack.tail();
                     stack <- stack.cons(temp).cons(temp2);
                     print_list(stack);
                  }    
               else
                  0
               fi fi;
            }
            else
               0
            fi;
         }
      else if (stack.head() = "d")
         then {
               copy <- stack.tail();
               stack <- stack.tail();
               while not stack.isNil() loop {
                  out_string(stack.head());
                  out_string("\n");
                  stack <- stack.tail();
               }
               pool;
               stack <- copy;    
         }
      else
         0
      fi fi
   };

   print_list(l : List) : Object {
      if l.isNil() then out_string("\n")
                   else {
			   out_string(l.head());
			   out_string(" ");
			   print_list(l.tail());
		        }
      fi
   };

   main() : Object {
      {   
         while not exitLoop() loop 
         {
               initCheck();
               out_string(">");
               input <- in_string();
               if (input) = "" then {
                  inputConv <- atoi(input);
                  input <- itoa(inputConv);
               }
               else
                  0
               fi;
               stack <- stack.cons(input);
               evaluate();
               input <- "";
         }
         pool;
      }
   };
};

(*
 *  This file shows how to implement a list data type for lists of integers.
 *  It makes use of INHERITANCE and DYNAMIC DISPATCH.
 *
 *  The List class has 4 operations defined on List objects. If 'l' is
 *  a list, then the methods dispatched on 'l' have the following effects:
 *
 *    isNil() : Bool		Returns true if 'l' is empty, false otherwise.
 *    head()  : String		Returns the string at the head of 'l'.
 *				If 'l' is empty, execution aborts.
 *    tail()  : List		Returns the remainder of the 'l',
 *				i.e. without the first element.
 *    cons(i : String) : List	Return a new list containing i as the
 *				first element, followed by the
 *				elements in 'l'.
 *
 *  There are 2 kinds of lists, the empty list and a non-empty
 *  list. We can think of the non-empty list as a specialization of
 *  the empty list.
 *  The class List defines the operations on empty list. The class
 *  Cons inherits from List and redefines things to handle non-empty
 *  lists.
 *)


class List {
   -- Define operations on empty lists.

   isNil() : Bool { true };

   -- Since abort() has return type Object and head() has return type
   -- Int, we need to have an Int as the result of the method body,
   -- even though abort() never returns.

   head()  : String { { abort(); ""; } };

   -- As for head(), the self is just to make sure the return type of
   -- tail() is correct.

   tail()  : List { { abort(); self; } };

   -- When we cons and element onto the empty list we get a non-empty
   -- list. The (new Cons) expression creates a new list cell of class
   -- Cons, which is initialized by a dispatch to init().
   -- The result of init() is an element of class Cons, but it
   -- conforms to the return type List, because Cons is a subclass of
   -- List.

   cons(i : String) : List {
      (new Cons).init(i, self)
   };

};


(*
 *  Cons inherits all operations from List. We can reuse only the cons
 *  method though, because adding an element to the front of an emtpy
 *  list is the same as adding it to the front of a non empty
 *  list. All other methods have to be redefined, since the behaviour
 *  for them is different from the empty list.
 *
 *  Cons needs two attributes to hold the integer of this list
 *  cell and to hold the rest of the list.
 *
 *  The init() method is used by the cons() method to initialize the
 *  cell.
 *)

class Cons inherits List {

   car : String;	-- The element in this list cell

   cdr : List;	-- The rest of the list

   isNil() : Bool { false };

   head()  : String { car };

   tail()  : List { cdr };

   init(i : String, rest : List) : List {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };

};



(*
 *  The Main class shows how to use the List class. It creates a small
 *  list and then repeatedly prints out its elements and takes off the
 *  first element of the list.
 *)




