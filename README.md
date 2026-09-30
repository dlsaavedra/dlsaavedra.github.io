# Sitio académico de Daniel Saavedra Morales

Código fuente de [dlsaavedra.github.io](https://dlsaavedra.github.io/), un sitio académico creado con [Jekyll](https://jekyllrb.com/) a partir del tema [al-folio](https://github.com/alshedivat/al-folio). El contenido público se genera desde este repositorio y se publica mediante GitHub Pages.

## Qué encontrarás en la web

| Pestaña | Contenido |
| --- | --- |
| **Home** | Biografía, intereses de investigación, fotografía y enlaces académicos. |
| **CV** | Vista del currículum en la página y enlace al PDF original. |
| **Publications** | Lista de publicaciones y enlace al perfil ORCID. |
| **Repositories** | Selección de repositorios públicos de GitHub y paquetes de R. |
| **Current Projects** | Tres proyectos de investigación con imágenes, descripciones y formulaciones matemáticas. |
| **Presentations** | Enlaces a presentaciones en PDF. Aparecen cuando se añaden archivos a la carpeta correspondiente. |
| **Teaching** | Cursos impartidos, sus páginas de materiales en PDF y ayudantías realizadas. |

## Dónde está cada cosa

| Ruta | Función |
| --- | --- |
| `_config.yml` | Nombre, URL, correo, redes, ORCID y configuración general. |
| `_pages/` | Contenido de las pestañas principales. |
| `_courses/` | Una página por curso, con sus datos y enlace a materiales. |
| `_projects/` | Una página por proyecto de investigación. |
| `_includes/`, `_layouts/`, `_sass/` | Componentes, plantillas y estilos del sitio. |
| `assets/img/` | Fotografía, imágenes de proyectos y vistas previas del CV. |
| `assets/pdf/daniel-saavedra-cv.pdf` | CV descargable. |
| `assets/pdf/presentations/` | Presentaciones: cada PDF añadido aparece en **Presentations**. |
| `assets/pdf/courses/<course_slug>/` | Materiales: cada PDF añadido aparece en la página del curso correspondiente. |
| `.github/workflows/deploy.yml` | Compilación y publicación automática en la rama `gh-pages`. |

Las publicaciones y los repositorios mostrados en la web son listas editadas en `_pages/publications.md` y `_pages/repositories.md`; no se sincronizan automáticamente con ORCID o GitHub.

## Tutorial: crear tu propia web a partir de este repositorio

No se necesita la herramienta `gh`: Git y la interfaz web de GitHub son suficientes.

### 1. Obtener una copia

En [este repositorio](https://github.com/dlsaavedra/dlsaavedra.github.io), selecciona **Code → Download ZIP** y descomprime el archivo. La copia descargada no incluye el historial de Git del sitio original.

En GitHub, crea un repositorio vacío llamado **`TU_USUARIO.github.io`**, sustituyendo `TU_USUARIO` por tu nombre de usuario real. No marques las opciones para crear README, `.gitignore` o licencia: esos archivos ya están en la copia. Ese nombre permite publicar el sitio en `https://TU_USUARIO.github.io/`.

### 2. Sustituir la información personal

Antes de publicar, revisa estos archivos:

1. En `_config.yml`, cambia `first_name`, `last_name`, `email`, `url`, `github_username`, `orcid_id` y los demás enlaces sociales. Para un repositorio `TU_USUARIO.github.io`, usa `url: https://TU_USUARIO.github.io` y deja `baseurl` vacío.
2. En `_pages/about.md`, cambia la biografía, el foco de investigación y la fotografía. Sustituye la imagen referida allí, actualmente `assets/img/daniel-saavedra.jpg`.
3. En `_pages/cv.md`, cambia el enlace al PDF y las imágenes de vista previa. Sustituye `assets/pdf/daniel-saavedra-cv.pdf`. La vista actual muestra **dos imágenes de página**; si tu CV tiene otro número de páginas, genera una imagen por página y ajusta las etiquetas `<img>` de `_pages/cv.md`.
4. Edita `_pages/publications.md`, `_pages/repositories.md` y `_pages/teaching.md` con tus propios datos. Revisa también `_courses/` y `_projects/`, pues contienen información académica de este sitio.
5. Sustituye o elimina archivos personales que no vayas a usar en `assets/img/` y `assets/pdf/`. Actualiza este README para describir tu versión.

### 3. Añadir contenido

- **Presentaciones:** guarda archivos `.pdf` en `assets/pdf/presentations/`. El nombre del archivo se convierte en el texto del enlace; utiliza nombres claros, por ejemplo `bayesian-extremes-2026.pdf`.
- **Materiales de cursos:** cada archivo de `_courses/` declara un `course_slug`. Guarda sus PDF en `assets/pdf/courses/<course_slug>/`. La página del curso los listará automáticamente.
- **Cursos nuevos:** crea una página Markdown en `_courses/` con `layout: page`, `title`, `institution`, `period`, `order` y `course_slug`; añade `{% raw %}{% include course_materials.html slug=page.course_slug %}{% endraw %}` donde quieras mostrar sus materiales.
- **Proyectos nuevos:** crea una página Markdown en `_projects/` con `layout: page`, `title`, `description`, `img` e `importance`; guarda su imagen en `assets/img/projects/`. La página **Current Projects** ordena las tarjetas por `importance`.
- **Pestañas:** se definen en `_pages/`. Sus campos `nav` y `nav_order` controlan si aparecen en el menú y su orden.

### 4. Vista local opcional

Instala Ruby y Bundler, entra en la carpeta descomprimida y ejecuta:

```bash
bundle install
bundle exec jekyll serve
```

Abre `http://localhost:4000/`. Este proyecto usa complementos de Jekyll que pueden requerir dependencias adicionales del sistema; la configuración de `.github/workflows/deploy.yml` muestra las herramientas utilizadas para la compilación publicada. Consulta también la [guía oficial de Jekyll](https://jekyllrb.com/docs/) si necesitas preparar Ruby en tu sistema.

### 5. Publicar en GitHub

Dentro de la carpeta descomprimida, después de reemplazar los datos personales:

```bash
git init
git branch -M main
git add .
git commit -m "Create my academic website"
git remote add origin https://github.com/TU_USUARIO/TU_USUARIO.github.io.git
git push -u origin main
```

La acción de `.github/workflows/deploy.yml` compila el sitio tras cada envío a `main` o `master` y coloca el resultado en `gh-pages`. Cuando exista esa rama, en el repositorio de GitHub ve a **Settings → Pages → Build and deployment** y selecciona **Deploy from a branch → `gh-pages` → `/(root)`**. Revisa la pestaña **Actions** para ver si la compilación terminó correctamente. La [documentación de GitHub Pages](https://docs.github.com/en/pages/getting-started-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site) explica esta configuración.

Si usas un nombre de repositorio distinto de `TU_USUARIO.github.io`, cambia `baseurl` en `_config.yml` a `/<nombre-del-repositorio>` y comprueba que los enlaces e imágenes se vean bien bajo esa ruta.

## Recomendaciones

- Sustituye el CV, la fotografía y los enlaces personales antes del primer `git push`: GitHub Pages publica el contenido del sitio de forma abierta.
- Usa nombres de archivo sin espacios ni tildes para PDF e imágenes; serán parte de las URL.
- Mantén los PDF de cada curso en su carpeta y revisa sus enlaces después de añadirlos.
- Comprueba en **Actions** que la compilación termine antes de diagnosticar un cambio que aún no aparece en la web.
- Conserva `LICENSE` y la atribución al tema [al-folio](https://github.com/alshedivat/al-folio) al reutilizar el código. El archivo `LICENSE` contiene su licencia MIT.
