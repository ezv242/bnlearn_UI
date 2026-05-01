function convertirInputDatasetSimple(inputId, nFilas) {

  let input = document.getElementById(inputId);

  // Soporte para wrappers (shiny.semantic)
  if (!input) {
    const wrapper = document.querySelector(`[id='${inputId}']`);
    if (wrapper) {
      input = wrapper.querySelector("input");
    }
  }

  if (!input) {
    console.log("No se encontró el input:", inputId);
    return;
  }

  input.type = "text";
  input.placeholder = "Haz click para seleccionar filas...";

  // WRAPPER (input con tags)
  let wrapper = input._wrapper;

  if (!wrapper) {
    wrapper = document.createElement("div");

    wrapper.style.display = "flex";
    wrapper.style.flexWrap = "wrap";
    wrapper.style.alignItems = "center";
    wrapper.style.gap = "5px";
    wrapper.style.padding = "4px";
    wrapper.style.border = "1px solid #ccc";
    wrapper.style.borderRadius = "4px";
    wrapper.style.minHeight = "38px";
    wrapper.style.cursor = "text";
    wrapper.style.background = "white";

    input.parentNode.insertBefore(wrapper, input);
    wrapper.appendChild(input);

    input._wrapper = wrapper;
  }

  // Estilo del input interno
  input.style.border = "none";
  input.style.outline = "none";
  input.style.flex = "1";
  input.style.minWidth = "120px";

  // CONTENEDOR DE TAGS
  let tagContainer = input._tagContainer;

  if (!tagContainer) {
    tagContainer = document.createElement("div");

    tagContainer.style.display = "flex";
    tagContainer.style.flexWrap = "wrap";
    tagContainer.style.gap = "5px";

    wrapper.insertBefore(tagContainer, input);

    input._tagContainer = tagContainer;
  } else {
    tagContainer.innerHTML = '';
  }

  // Click en todo el wrapper abre el popup
  wrapper.addEventListener("click", () => input.click());

  // EVITAR DUPLICAR EVENTOS
  if (input._clickHandler) {
    input.removeEventListener("click", input._clickHandler);
  }

  // POPUP
  input._clickHandler = function () {

    const popupId = "popupFilas_" + inputId;

    let existing = document.getElementById(popupId);
    if (existing) existing.remove();

    let popup = document.createElement("div");
    popup.id = popupId;

    popup.style.position = "fixed";
    popup.style.background = "rgba(255,255,255,0.97)";
    popup.style.border = "2px solid rgba(0,0,0,0.6)";
    popup.style.padding = "8px";
    popup.style.zIndex = 9999;
    popup.style.maxHeight = "300px";
    popup.style.overflow = "auto";
    popup.style.boxShadow = "0 4px 14px rgba(0,0,0,0.25)";

    popup.addEventListener("click", (e) => e.stopPropagation());

    // Botón seleccionar todas
    let allBtn = document.createElement("button");
    allBtn.textContent = "Seleccionar todas las filas";
    allBtn.style.display = "block";
    allBtn.style.marginBottom = "6px";

    allBtn.onclick = (e) => {
      e.stopPropagation();

      for (let i = 0; i < nFilas; i++) {
        if (!Array.from(tagContainer.children).some(t => parseInt(t.dataset.index) === i)) {
          addTag(i);
        }
      }

      popup.remove();
    };

    popup.appendChild(allBtn);

    // Botones individuales
    for (let i = 0; i < nFilas; i++) {

      if (Array.from(tagContainer.children).some(t => parseInt(t.dataset.index) === i)) continue;

      let btn = document.createElement("button");
      btn.textContent = `Fila ${i + 1}`;
      btn.style.margin = "2px";

      btn.onclick = (e) => {
        e.stopPropagation();
        addTag(i);
        popup.remove();
      };

      popup.appendChild(btn);
    }

    document.body.appendChild(popup);

    const rect = wrapper.getBoundingClientRect();
    popup.style.left = rect.left + "px";
    popup.style.top = rect.bottom + "px";

    // Cerrar al hacer click fuera
    setTimeout(() => {
      function handler(e) {
        if (!popup.contains(e.target)) {
          popup.remove();
          document.removeEventListener("click", handler);
        }
      }
      document.addEventListener("click", handler);
    }, 0);
  };

  input.addEventListener("click", input._clickHandler);

  // ============================
  // ➕ AÑADIR TAG
  // ============================

  function addTag(filaIndex) {
    const tag = document.createElement("span");

    tag.textContent = `Fila ${filaIndex + 1}`;
    tag.dataset.index = filaIndex;

    tag.style.background = "#2185d0";
    tag.style.color = "white";
    tag.style.padding = "3px 8px";
    tag.style.borderRadius = "12px";
    tag.style.cursor = "pointer";
    tag.style.fontSize = "12px";

    tag.onclick = () => {
      tagContainer.removeChild(tag);
      updateInput();
    };

    tagContainer.appendChild(tag);
    updateInput();
  }

  // ============================
  // 🔄 SINCRONIZAR CON SHINY
  // ============================

  function updateInput() {
    const seleccionadas = Array.from(tagContainer.children).map(t =>
      parseInt(t.dataset.index)
    );

    Shiny.setInputValue(inputId, seleccionadas, { priority: "event" });
  }
}

// ============================
// 🔗 HANDLER SHINY
// ============================

Shiny.addCustomMessageHandler("convertirInputDatasetSimple", function (msg) {
  convertirInputDatasetSimple(msg.inputId, msg.nFilas);
});