var tds=document.getElementsByClassName("cellinside");
for (var i=0;i<tds.length;i++) {
    if (parseFloat(tds[i].innerHTML)<1 && parseFloat(tds[i].innerHTML)>0 ){
        tds[i].parentNode.className="change";
        }
}

var tds=document.getElementsByClassName("cellinside");
for (var i=0;i<tds.length;i++) {
    if (tds[i].innerHTML.indexOf("reset")>=0){
        tds[i].parentNode.className="reset";
    }
}

var tds=document.getElementsByClassName("cellinside");
for (var i=0;i<tds.length;i++) {
    if (tds[i].innerHTML.indexOf("not rel.")>=0){
        tds[i].parentNode.className="notrel";
    }
}

var tds=document.getElementsByClassName("cellinside");
for (var i=0;i<tds.length;i++) {
    if (tds[i].innerHTML.indexOf("ins.")>=0){
        tds[i].parentNode.className="insertion";
    }
}

var tds=document.getElementsByClassName("cellinside");
for (var i=0;i<tds.length;i++) {
    if (tds[i].innerHTML.indexOf("del.")>=0){
        tds[i].parentNode.className="deletion";
    }
}

// moving <tr class = firstline> nodes to thead of table to make stickyTableHeaders work
var firstLines=document.getElementsByClassName("firstline");
for (var i=0; i<firstLines.length; i++) {
        var table = firstLines[i].parentNode.parentNode;
          tHead = table.insertBefore(document.createElement('thead'), table.firstChild );
          tHead.appendChild( table.querySelector('tr.firstline') );
}


// making 0 to proNone; 1 to proMaj; and 2 to proMin
var diffs = document.evaluate("/html/body/p/table/tbody/tr/td/table/tbody/tr/td[8]",document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null );

for ( var i=0 ; i < diffs.snapshotLength; i++ )
{
    if(parseFloat(diffs.snapshotItem(i).textContent)==1){
        diffs.snapshotItem(i).textContent="proMaj" ; 
    }
    if(parseFloat(diffs.snapshotItem(i).textContent)==0){
        diffs.snapshotItem(i).textContent="proNone" ; 
    }
    if(parseFloat(diffs.snapshotItem(i).textContent)==2){
        diffs.snapshotItem(i).textContent="proMin" ; 
    }
}









