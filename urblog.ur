style quote
style game
style hide
style cellclean
style stats
style prompt
style answer
style addpoints
style subtractpoints	
style options
style super
style classname
style sel
style tst

table entry : { Channel : channel (string * string * int)  }

	
type question = { Question : string , Answer : string, Point : int  }

type category = { Nam : string, Questions : list question }

type game =  list category   


 

fun sendChannel v =
    r <- oneRow (SELECT entry.Channel FROM entry);
    send r.Entry.Channel v



fun professor () =
    ch <- channel;
    dml (INSERT INTO entry ( Channel) VALUES ( {[ch]}));
    answerReal <- source " ";
    question <- source " ";
	lol <- source 10;

    let
        fun receiver () =
            v <- recv ch;
	    set answerReal (v.1);
	    set question ( v.2 );
            receiver ()


	

    in
        return <xml>

  <head>
    <link rel="stylesheet" type="text/css" href="http://mit.edu/anishd/www/test.css"/>

    <title>Ur/web Professor Page</title>
  </head>

<body onload={spawn (receiver ())}>

<div class={quote}>	
    <h1>Ur/Web Jepoardy Professor Page</h1>
</div>
	<div class={prompt}>
    <dyn signal={w <- signal question; return <xml><h2 class = {answer}>The current question is: {[w]}</h2></xml>}/>

<p>

    <dyn signal={x <- signal answerReal; return <xml><h2 class = {answer}>The current answer is: {[x]}</h2></xml>}/>
</p>

	</div>
</body>

	      </xml>
    end


fun length [a] (ls : list a) : int =
    case ls of
        [] => 0
      | _ :: ls' => 1 + length ls'



fun attack n t = if n = t then 1 else n + 1



fun test n = if n = 25 then True else False

fun increment n = n + 1


val col : category = { Nam = "First Bank of Quest" , Questions = { Question = "1 Question", Answer = " 1 Answer", Point = 100}  :: { Question = "2 Question", Answer = "2 Answer", Point = 100} :: { Question = "3 Question", Answer = "3 Answer", Point = 100} :: { Question = "4 Question", Answer = "4 Answer", Point = 100} :: { Question = "5 Question", Answer = "5 Answer", Point = 100} :: [] }  

val col1 : category = { Nam = "Second Bank of Quest" , Questions = { Question = "1.1 Question", Answer = " 1.1 Answer", Point = 200}  :: { Question = "2.1 Question", Answer = "2.1 Answer", Point = 200} :: { Question = "3.1 Question", Answer = "3.1 Answer", Point = 200} :: { Question = "4.1 Question", Answer = "4.1 Answer", Point = 200} :: { Question = "5.1 Question", Answer = "5.1 Answer", Point = 200} :: [] }  

val col2 : category = { Nam = "Third Bank of Quest" , Questions = { Question = "1.2 Question", Answer = " 1.2 Answer", Point = 300}  :: { Question = "2.2 Question", Answer = "2.2 Answer", Point = 300} :: { Question = "3.2 Question", Answer = "3.2 Answer", Point = 300} :: { Question = "4.2 Question", Answer = "4.2 Answer", Point = 300} :: { Question = "5.2 Question", Answer = "5.2 Answer", Point = 300} :: [] }  

val col3 : category = { Nam = "Fourth Bank of Quest" , Questions = { Question = "1.3 Question", Answer = " 1.3 Answer", Point = 400}  :: { Question = "2.3 Question", Answer = "2.3 Answer", Point = 400} :: { Question = "3.3 Question", Answer = "3.3 Answer", Point = 400} :: { Question = "4.3 Question", Answer = "4.3 Answer", Point = 400} :: { Question = "5.3 Question", Answer = "5.3 Answer", Point = 400} :: [] }  

val col4 : category = { Nam = "Fifth Bank of Quest" , Questions = { Question = "1.4 Question", Answer = " 1.4 Answer", Point = 500}  :: { Question = "2.4 Question", Answer = "2.4 Answer", Point = 500} :: { Question = "3.4 Question", Answer = "3.4 Answer", Point = 500} :: { Question = "4.4 Question", Answer = "4.4 Answer", Point = 500} :: { Question = "5.4 Question", Answer = "5.4 Answer", Point = 500} :: [] }  



fun main () = 
 
       

x3 <- source 0;
x2 <- source 0;
x4 <- source 0;
x5 <- source 0;
x6 <- source 0;
on <- source 0;
games <- source 0;
check <- source 1;
s <- source " ";
questionT <- source " ";
answerT <- source " ";
viewQuestion <- source False;
viewAnswer <- source False;
two <- source False;
viewPlayer <- source True;
gameOn <- source False;
gameOver <- source False;
players <- source [];
m<- source False;
m2 <- source False;
m3<- source False;
testing <- source " ";
m4 <- source False;
g <- return ( col :: col1 :: col2 :: col3 :: col4 :: [] );
srcs <- source [];
supa <- source " ";
tm <- now;
count <- source " ";






g <- List.mapM (fn category => 
	questions <- List.mapM (fn question => 
		attempt <- source False; return (question ++ {Attempt = attempt}) ) category.Questions; 
	return { Nam = category.Nam, Questions = questions} ) g;



tennis <- source 10;
first <- source True;
cancel <- source first;
    let
        fun looping prefix delay check =
		
            let
                fun looping' n =
		    x <- get check;
		if x then
		    set tennis n;
                    sleep delay;
                    looping' (n - 1)
		else return ()
            in
                looping'
            end
    in
return
 <xml>
  <head>
    <link rel="stylesheet" type="text/css" href="http://mit.edu/anishd/www/test.css"/>

    <title>Ur/web</title>
  </head>

  <body>											

<div class={quote} >
    <h1>Ur/Web Jepoardy</h1>


<dyn signal = {turn <- signal two; 
	if  turn then
		return
<xml>
  <dyn signal={x <- signal supa; return <xml>It is currently {[x]}'s turn!</xml>}/>
</xml>		

else return <xml/>}/>
</div>

<dyn signal = {thePlayer <- signal viewPlayer; 
	if thePlayer then
		return
<xml>

<div class ={options}>
<h1> Ur/Web Jeopardy </h1>
<label> Number of Teams </label>	

<h1>2 Teams</h1>
<ccheckbox source = {m} onclick={set players (1 :: 2 :: []);set srcs (x2 :: x3 :: []);  set m2 False; set m3 False; set m4 False; set two True}/>
<h1>3 Teams</h1>
<ccheckbox source={m2} onclick={set players (1 :: 2 :: 3 :: []);set srcs (x2 :: x3 :: x4 :: []); set m3 False; set m4 False; set m False; set two True }/>
<h1>4 Teams</h1>
<ccheckbox source={m3} onclick={set players (1 :: 2 :: 3 :: 4 :: []);set srcs (x2 :: x3 :: x4 :: x5 :: []); set m4 False; set m False; set m2 False; set two True}/>
<h1>5 Teams</h1>
<ccheckbox source={m4} onclick={set players (1 :: 2 :: 3 :: 4 :: 5 :: []);set srcs (x2 :: x3 :: x4 :: x5 :: x6 :: []); set m2 False; set m3 False; set m False; set two True}/>		
     
<dyn signal={s <- signal players; return <xml>
<label> You are currently playing with <h1>{ [length s] } teams</h1> </label>


</xml>}/>


      <br/>




  <form target={_blank}>

<submit value="Professor Page!" action={professor}/></form>

<br></br>

<button value="Begin the game!" onclick={set viewPlayer False; set gameOn True}/> 





</div>


</xml>

else return <xml/>}/>
	

<dyn signal = {viewOn <- signal viewQuestion;
	if viewOn then

		return 
<xml>	
<div class={prompt}>
	

	 <dyn signal={x1 <- signal questionT; return <xml><h2 class = {answer}>{[x1]}</h2></xml>}/>
	<p> 

	<button value="Show Me The Answer!" onclick={set viewAnswer True}/>  <br/>

	<dyn signal = {displayA <- signal viewAnswer;
		if displayA then
			return <xml>
    <dyn signal={v <- signal answerT;x <- signal check;m <- signal players;overT <- signal games; return <xml>{[v]}</xml>}/>
	</xml>
		else return <xml/>}/>
	</p>

<a class={classname} onclick={set viewQuestion False; set answerT " "; set viewAnswer False;overT <- get games; if (test overT) then set gameOn False else set gameOn True; set games (increment overT)}>I Got It Right!</a>
(*
<a class={classname} onclick={set viewQuestion False; set answerT " "; set viewAnswer False; x <- get check;m <- get players; set check (attack x (length m));overT <- get games; if (test overT) then set gameOn False else set gameOn True; set games (increment overT)}>Next Turn!</a>*)

<p> Time remaining to answer question: </p>
<dyn signal={s <- signal tennis; return <xml>
<label> <h1>{ [s] } seconds</h1> </label>

</xml>}/>
</div>	
</xml>
	else return <xml/>}/>



<dyn signal = {overT <- signal games;
	if (test overT) then

		return 
<xml>	
<div class={prompt}>
	


	
	

	 <h2 class = {answer}>Game Over!</h2>


	<p> Thanks for playing! </p>

	
  																																						

</div>
</xml>
	else return <xml/>}/>




<dyn signal = {viewTheGame <- signal gameOn;
	if viewTheGame then

		return 
<xml>	



<div class={game}>


<table>

		


		<tr class = { super } >
			
			{

	  			
			List.mapX (fn category => <xml> <th> {[category.Nam]}</th>  </xml>)
			g
			
	
			}
		</tr> 
			

{


List.mapX (fn outer => <xml>
<tr>

{List.mapX (fn loop =>	<xml>





<td class = { cellclean } onclick = { set gameOn False ; set viewQuestion True; set questionT loop.Question; set answerT loop.Answer; set on loop.Point; set loop.Attempt True; rpc (sendChannel (loop.Answer, loop.Question, loop.Point)); spawn ( check <- source True; x <- get cancel; set x False; set cancel check;  looping  "A" 1000 check 10  )} >




<dyn signal = {attempt <- signal loop.Attempt; 
	if not attempt then		

		return

	<xml>




<h3> {[loop.Point]} </h3>



</xml>
	else return <xml/>}/>



</td>



</xml> ) outer.Questions }
</tr>

</xml> ) g 
	

	}
		</table>		

</div>		

	
</xml>
	else return <xml/>}/>	
<div class={stats}>
	<dyn signal={s <- signal players; return <xml>
<div class ={sel}>
<p> Select whose turn it is: </p>
</div>
<cselect source={supa} class = {tst}>

{List.mapX (fn opt => <xml>

        <coption>Team {[opt]}</coption>
</xml> ) s }

      </cselect>
</xml>}/>
<table>

<tr>


	<dyn signal={s <- signal players; return <xml>



{List.mapX (fn super => <xml>


<th>
<h3> Team {[super]} </h3>
</th>
</xml> ) s }

</xml>}/>



</tr>


<tr>


<dyn signal = { t <- signal srcs; return <xml>

{List.mapX (fn sources => <xml> 

<td>

<dyn signal={v <- signal sources; t <- signal on; return <xml><h4>{[v]} </h4></xml>}/>
<button value="+" class={addpoints} onclick={v <- get sources	; t <- get on; set sources ( v + t)}/>  <br/>
	
<button value="-" class={subtractpoints} onclick={v <- get sources;t <- get on; set sources (v - t)}/>  <br/>

</td>
</xml> ) t }

</xml>}/>

</tr>

</table>
</div>


  </body>
</xml>
    end
