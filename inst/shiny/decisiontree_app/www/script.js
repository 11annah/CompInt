var check=0;

function check_input1(p1, p2, p3) {
    var s = document.getElementById(p1).innerHTML;
    if (s.search("'") < 0) {
        alert("Invalid " + p2 + "!");
        return "?";
    } else {
        var pos = s.indexOf("'");
        if (pos !== -1) {
            s = s.substring(pos + 1); // Get the substring after the first '
            pos = s.indexOf("'");
            if (pos !== -1) {
                s = s.substring(0, pos); // Get the substring before the next '
                document.getElementById(p3).innerHTML = s; // Replace the innerHTML of p3
                return s;
            }
        }
    }
}
function check_inpu2(p1)
{if(document.getElementById("C"+p1.toString()).checked)
  {choice_s=choice_s+"<span style='color:"+document.getElementById("D"+choice_p.toString()).style.color+"'>Choice "+p1.toString()+"</span>"+"<br>";
   document.getElementById("D"+choice_p.toString()).innerHTML="Choice "+p1.toString();
   ++choice_p;
  }
}

function create_result()
{check=0;
 choice_s="";
 choice_p=5;
 s1=check_input1("click_message","Quantity","D2");
 s2=check_input1("click_message_assumption","Assumption","D3");
 s3=check_input1("click_message_distribution","Distribution","D4");
 for(i=1;i<5;++i){document.getElementById("D"+choice_p.toString()).style.visibility="visible";check_inpu2(i);}
 for(;choice_p<9;++choice_p)document.getElementById("D"+choice_p.toString()).style.visibility="hidden";
 document.getElementById("result").innerHTML=
  "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt "+
  "ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo "+
  "<br>"+"<span style='color:#FF0000'>"+s1+"</span>"+"<br>"+
  "dolores et ea rebum."+
  "<br>"+"<span style='color:#0000FF'>"+s2+"</span>"+"<br>"+
  "Stet clita kasd gubergren, no sea"+
  "<br>"+"<span style='color:#008000'>"+s3+"</span>"+"<br>"+
  "takimata sanctus est "+
  "<br>"+choice_s+"<br>"+
  "Lorem ipsum dolor sit amet.";
}
