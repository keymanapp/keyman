/* File upload via browser */

function handleFiles(files) {
  ([...files]).forEach(uploadFile);
}

function uploadFile(file) {
  let url = 'upload';
  let formData = new FormData();

  formData.append('file', file);

  fetch(url, {
    method: 'POST',
    body: formData
  })
  .then((res) => {
    if(!res.ok) {
      showToast('Unrecognised file name format "'+file.name+'".', 'error');
    } else {
      showToast('Uploaded "'+file.name+'" successfully.', 'success');
    }
  })
  .catch((e) => {
    showToast('Network error uploading "'+file.name+'": '+e+'.', 'error');
  });
}

const toastContainer = document.getElementById('toast-container');
const toastTemplates = {
  success: document.getElementById('toast-success-template'),
  info: document.getElementById('toast-info-template'),
  error: document.getElementById('toast-error-template'),
};

function showToast(message, type) {
  let tt = toastTemplates[type].cloneNode(true);
  tt.id = '';
  tt.querySelector('.toast-body').innerText = message;
  tt.addEventListener('hidden.bs.toast', function() {
    toastContainer.removeChild(tt);
  });
  toastContainer.appendChild(tt);
  let toast = new bootstrap.Toast(tt);
  toast.show();
}

(function() {
  let dropArea = document.getElementById('drop-area');
  ['dragenter', 'dragover', 'dragleave', 'drop'].forEach(eventName => {
    dropArea.addEventListener(eventName, preventDefaults, false)
  })

  function preventDefaults (e) {
    e.preventDefault();
    e.stopPropagation();
  }

  ['dragenter'].forEach(eventName => {
    dropArea.addEventListener(eventName, highlight, false);
  });

  ;['dragleave'].forEach(eventName => {
    dropArea.addEventListener(eventName, unhighlight, false);
  });

  let dragDepth = 0;

  function highlight(e) {
    dragDepth++;
    dropArea.classList.add('highlight');
  }

  function unhighlight(e) {
    if(--dragDepth <= 0) {
      dragDepth = 0;
      dropArea.classList.remove('highlight');
    }
  }

  dropArea.addEventListener('drop', handleDrop, false);

  document.getElementById('drop-button').addEventListener('click', () => {
    document.getElementById('drop-file').click();
  });

  function handleDrop(e) {
    let dt = e.dataTransfer;
    let files = dt.files;
    unhighlight(e);
    handleFiles(files);
  }

})();