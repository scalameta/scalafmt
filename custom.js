Array
  .from(document.getElementsByClassName('scalafmt-configuration-toggle'))
  .forEach(function(elem) {
    elem.onclick = function() {
      elem.parentElement.classList.toggle('collapsed');
    }
  });
