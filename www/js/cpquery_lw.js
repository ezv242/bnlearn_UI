function convertirInputDatasetSimple(inputId, nFilas) {
  const input = document.getElementById(inputId);
  if (!input) return;

  input.type = "text";
  input.placeholder = "Haz click para seleccionar filas...";

  // Contenedor de etiquetas (reutilizable)
  let tagContainer = input._tagContainer;
  if (!tagContainer) {
    tagContainer = document.createElement("div");
    tagContainer.style.display = "flex";
    tagContainer.style.flexWrap = "wrap";
    tagContainer.style.gap = "5px";
    tagContainer.style.marginTop = "5px";
    input.parentNode.insertBefore(tagContainer, input.nextSibling);
    input._tagContainer = tagContainer;
  }

  // Evitar múltiples listeners
  if (input._clickHandler) input.removeEventListener("click", input._clickHandler);

  input._clickHandler = function () {
    // Eliminar popup previo
    let existing = document.getElementById("popupFilas");
    if (existing) existing.remove();

    // Crear popup
    let popup = document.createElement("div");
    popup.id = "popupFilas";
    popup.style.position = "fixed";
    popup.style.background = "rgba(255,255,255,0.97)";
    popup.style.border = "2px solid rgba(0,0,0,0.6)";
    popup.style.padding = "8px";
    popup.style.zIndex = 9999;
    popup.style.maxHeight = "300px";
    popup.style.overflow = "auto";
    popup.style.boxShadow = "0 4px 14px rgba(0,0,0,0.25)";
    popup.addEventListener("click", (e) => e.stopPropagation());

    // Botón “Seleccionar todas las filas”
    let allBtn = document.createElement("button");
    allBtn.textContent = "Seleccionar todas las filas";
    allBtn.style.display = "block";
    allBtn.style.marginBottom = "6px";
    allBtn.onclick = (e) => {
      e.stopPropagation();
      for (let i = 0; i < nFilas; i++) {
        // Solo crear si no está seleccionada
        if (!Array.from(tagContainer.children).some(t => t.textContent === `Fila ${i + 1}`)) {
          addTag(i);
        }
      }
      popup.remove();
    };
    popup.appendChild(allBtn);

    // Botones por fila
    for (let i = 0; i < nFilas; i++) {
      // Comprobar si ya existe etiqueta para esta fila
      if (Array.from(tagContainer.children).some(t => t.textContent === `Fila ${i + 1}`)) continue;

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
    const rect = input.getBoundingClientRect();
    popup.style.left = rect.left + "px";
    popup.style.top = rect.bottom + "px";

    // Cerrar popup al click fuera
    setTimeout(() => {
      document.addEventListener("click", function handler(e) {
        if (!popup.contains(e.target)) {
          popup.remove();
          document.removeEventListener("click", handler);
        }
      });
    }, 0);
  };

  input.addEventListener("click", input._clickHandler);

  // Función para agregar etiquetas
  function addTag(filaIndex) {
    const tag = document.createElement("span");
    tag.textContent = `Fila ${filaIndex + 1}`;
    tag.dataset.index = filaIndex;

    tag.style.background = "#007bff";
    tag.style.color = "white";
    tag.style.padding = "2px 6px";
    tag.style.borderRadius = "4px";
    tag.style.cursor = "pointer";

    tag.onclick = () => {
      tagContainer.removeChild(tag);
      updateInput();
    };

    tagContainer.appendChild(tag);
    updateInput();
  }

  // Sincronizar con Shiny
  function updateInput() {
    const seleccionadas = Array.from(tagContainer.children).map(t =>
      parseInt(t.textContent.replace("Fila ", "")) - 1
    );
    Shiny.setInputValue(inputId, seleccionadas, { priority: "event" });
  }
}

// Handler Shiny
Shiny.addCustomMessageHandler("convertirInputDatasetSimple", function (msg) {
  convertirInputDatasetSimple(msg.inputId, msg.nFilas);
});