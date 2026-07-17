// Progressive enhancement of the before/after blocks emitted by
// ScalafmtModifier. Each `.scalafmt-pair` renders original and formatted side
// by side; here we collapse it into a toggle that shows the formatted result
// by default and reveals the original on click. With JS disabled both blocks
// stay visible (the PR1 fallback), so this only ever adds behavior.

function enhance() {
  document
    .querySelectorAll(".scalafmt-pair:not([data-toggle])")
    .forEach((pair) => {
      const before = pair.querySelector(".before");
      const after = pair.querySelector(".after");
      if (!before || !after) return;
      pair.setAttribute("data-toggle", "");
      pair.classList.add("toggle");

      let formatted = true;
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className =
        "scalafmt-compare__toggle button button--sm button--outline button--primary";
      const render = () => {
        before.hidden = formatted;
        after.hidden = !formatted;
        btn.textContent = formatted ? "Show original" : "Show formatted";
      };
      btn.addEventListener("click", () => {
        formatted = !formatted;
        render();
      });
      render();
      pair.appendChild(btn);
    });
}

// Fires after hydration on first load and after every client-side navigation.
export function onRouteDidUpdate() {
  if (typeof document === "undefined") return;
  requestAnimationFrame(enhance);
}
