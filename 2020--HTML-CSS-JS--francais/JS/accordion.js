function acco() {
  if (this.classList.contains('active')) {
    clearAcco();
  }
  else {
    clearAcco();

    this.classList.add("active");
    var panel = this.nextElementSibling;
    panel.style.display = "block"
  }
}

function clearAcco() {
  var panelElems = document.getElementsByClassName("panel");
  for (var i = 0; i < panelElems.length; i++) {
    panelElems[i].style.display = "none";
  }

  var accElems = document.getElementsByClassName("accordion");
  for (var i = 0; i < accElems.length; i++) {
    accElems[i].classList.remove("active");
  }
}
