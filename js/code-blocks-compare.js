// Turn off ESLint for this file because it's sent down to users as-is.
/* eslint-disable */
window.addEventListener('load', function() {
  function button(label, ariaLabel) {
    const btn = document.createElement('button');
    btn.classList.add('btnCompare');
    btn.setAttribute('type', 'button');
    btn.setAttribute('aria-label', ariaLabel);
    btn.innerHTML =
      '<div class="btnCompare__body">' +
      '<strong class="btnCompare__label">' +
      label +
      '</strong>' +
      '</div>';
    return btn;
  }

  function addButtons(codeBlockSelector, btn, onClick) {
    document.querySelectorAll(codeBlockSelector).forEach(function(code) {
      code.parentNode.appendChild(btn.cloneNode(true));
    });
  }

  addButtons(
    '.hljs.formatted',
    button('Show original', 'Show original code')
  );

  addButtons(
    '.hljs.original',
    button('Show formatted', 'Show formatted code')
  )

  document.querySelectorAll('button.btnCompare').forEach(function(e) {
    e.addEventListener('click', function(e) {
      var preBlockToHide = e.target.closest('pre');
      var preBlockToShow = preBlockToHide.firstChild.classList.contains('formatted')
        ? preBlockToHide.nextElementSibling
        : preBlockToHide.previousElementSibling;
      preBlockToShow.classList.remove('hidden');
      preBlockToHide.classList.add('hidden');
    })
  });

  document.querySelectorAll('.hljs.original').forEach(function(e) {
    e.parentElement.classList.add('hidden');
  })

});
