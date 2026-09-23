// ==============================================================================
// herdr — Interactive Client Behaviors & Performance Fixes
// ==============================================================================

// 1. Button Loading State Handler
Shiny.addCustomMessageHandler('herdr_button_state', function(msg) {
  var btn = document.getElementById(msg.id);
  if (!btn) return;
  if (!btn.dataset.herdrLabel) btn.dataset.herdrLabel = btn.innerHTML;
  if (msg.loading) {
    btn.innerHTML = '<i class="fa fa-circle-notch fa-spin"></i> ' + (msg.text || 'Running...');
    btn.disabled = true;
  } else {
    btn.innerHTML = btn.dataset.herdrLabel;
    btn.disabled = false;
  }
});

// 2. Prevent scroll chaining when scrolling Handsontable dropdowns
function stopScrollChaining(e) {
  var isListbox = e.target.closest('.handsontable.listbox');
  if (isListbox) {
    e.stopPropagation();
    return;
  }
  var isTable = e.target.closest('.handsontable');
  if (isTable) {
    var holder = e.target.closest('.wtHolder');
    if (holder && holder.scrollHeight > holder.clientHeight) {
      var atTop = holder.scrollTop <= 0 && e.deltaY < 0;
      var atBottom = (holder.scrollHeight - holder.clientHeight - holder.scrollTop) <= 2 && e.deltaY > 0;
      if (atTop || atBottom) e.preventDefault();
    }
  }
}
document.addEventListener('wheel', stopScrollChaining, { passive: false, capture: true });

// 3. Tab Resize Fix: re-render Handsontables when switching tabs
document.addEventListener('DOMContentLoaded', function() {
  document.addEventListener('shown.bs.tab', function(event) {
    window.dispatchEvent(new Event('resize'));
    setTimeout(function() {
      var tables = document.querySelectorAll('.handsontable');
      tables.forEach(function(el) {
        if (window.$ && $(el).handsontable) {
          var hotInstance = $(el).handsontable('getInstance');
          if (hotInstance) {
            hotInstance.render();
          }
        }
      });
    }, 50);
  });

  // 4. Disable client-side red cell invalidation globally
  function disarmHandsontableValidators() {
    if (window.Handsontable) {
      if (Handsontable.validators) {
        Object.keys(Handsontable.validators).forEach(function(key) {
          Handsontable.validators[key] = function(val, cb) { cb(true); };
        });
      }
      if (Handsontable.hooks) {
        Handsontable.hooks.add('afterValidate', function() {
          return true;
        });
        Handsontable.hooks.add('afterRenderer', function(TD) {
          if (TD && TD.classList && TD.classList.contains('htInvalid')) {
            TD.classList.remove('htInvalid');
          }
        });
      }
    }
  }
  disarmHandsontableValidators();
  var hotCheckAttempts = 0;
  var hotCheckInterval = setInterval(function() {
    hotCheckAttempts++;
    if (window.Handsontable) {
      disarmHandsontableValidators();
      clearInterval(hotCheckInterval);
    }
    if (hotCheckAttempts > 50) clearInterval(hotCheckInterval);
  }, 200);
});

