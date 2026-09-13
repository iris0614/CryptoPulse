(function () {
  function activate(tab) {
    document.querySelectorAll(".cp-nav-link").forEach(function (el) {
      el.classList.toggle("is-active", el.getAttribute("data-tab") === tab);
    });
  }

  document.addEventListener("click", function (event) {
    var link = event.target.closest(".cp-nav-link");
    if (!link) {
      return;
    }
    event.preventDefault();
    var tab = link.getAttribute("data-tab");
    activate(tab);
    if (window.Shiny) {
      Shiny.setInputValue("nav_tab", tab, { priority: "event" });
    }
  });

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("cp-activate-nav", activate);
  }

  document.addEventListener("keydown", function (event) {
    var search = document.getElementById("navSearch");
    if ((event.metaKey || event.ctrlKey) && event.key === "k" && search) {
      event.preventDefault();
      search.focus();
    }
  });
})();
