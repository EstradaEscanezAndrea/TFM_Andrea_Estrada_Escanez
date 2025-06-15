document.querySelectorAll('.menu-trigger').forEach(trigger => {
    trigger.addEventListener('click', function (e) {
      // Cierra todos los menús primero
      document.querySelectorAll('.menu-opciones').forEach(menu => {
        if (menu !== this.nextElementSibling) {
          menu.style.display = 'none';
        }
      });

      // Alterna el menú correspondiente
      const menu = this.nextElementSibling;
      menu.style.display = (menu.style.display === 'flex') ? 'none' : 'flex';
    });
  });

  // Cierra menús si haces clic fuera
  document.addEventListener('click', function (e) {
    if (!e.target.closest('.descarga-menu')) {
      document.querySelectorAll('.menu-opciones').forEach(menu => {
        menu.style.display = 'none';
      });
    }
  });
