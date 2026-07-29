(function () {
  "use strict";

  function reportUnavailable() {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("download_unavailable", Date.now(), {
        priority: "event"
      });
    }
  }

  function responseFilename(response, link) {
    var header = response.headers.get("Content-Disposition") || "";
    var utf8 = header.match(/filename\*=UTF-8''([^;]+)/i);
    var basic = header.match(/filename="?([^";]+)"?/i);

    if (utf8) {
      try {
        return decodeURIComponent(utf8[1]);
      } catch (error) {
        return utf8[1];
      }
    }
    return basic ? basic[1] : (link.getAttribute("download") || "download");
  }

  document.addEventListener("click", async function (event) {
    var link = event.target.closest("a.shiny-download-link");
    if (!link) return;

    event.preventDefault();
    event.stopImmediatePropagation();
    if (link.dataset.downloading === "true") return;

    link.dataset.downloading = "true";
    link.setAttribute("aria-disabled", "true");

    try {
      var response = await fetch(link.href, { credentials: "same-origin" });
      var file = await response.blob();
      if (!response.ok || file.size === 0) {
        reportUnavailable();
        return;
      }

      var url = URL.createObjectURL(file);
      var saveLink = document.createElement("a");
      saveLink.href = url;
      saveLink.download = responseFilename(response, link);
      document.body.appendChild(saveLink);
      saveLink.click();
      saveLink.remove();
      setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
    } catch (error) {
      reportUnavailable();
    } finally {
      delete link.dataset.downloading;
      link.removeAttribute("aria-disabled");
    }
  }, true);
}());
