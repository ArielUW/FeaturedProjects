//explication: nous avons utilisé le tutorial de w3schools,
//recommandé pendant le cours:
//https://www.w3schools.com/howto/howto_js_slideshow.asp

//***** AVIS ETUDIANT SLIDESHOW *****
var slideIndex = 1;

function afficherSlides(n) {
  var i;
  var slides = document.getElementsByClassName("slide");
  var dots = document.getElementsByClassName("dot");
  if (n > slides.length) {slideIndex = 1}
  if (n < 1) {slideIndex = slides.length}
  for (i = 0; i < slides.length; i+=1) {
      slides[i].style.display = "none";
  }
  for (i = 0; i < dots.length; i+=1) {
      dots[i].className = dots[i].className.replace("actif", "");
  }
  slides[slideIndex-1].style.display = "block";
  dots[slideIndex-1].className += " actif";
}

//les dots
function leSlide(n) {
  afficherSlides(slideIndex = n);
}

//les flèches
function changerSlide(n) {
  afficherSlides(slideIndex += n);
}
