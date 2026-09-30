# Daniel Saavedra Morales — Academic Website

Source code for [dlsaavedra.github.io](https://dlsaavedra.github.io/), an academic website built with [Jekyll](https://jekyllrb.com/) and based on the [al-folio](https://github.com/alshedivat/al-folio) theme. GitHub Pages hosts the published site.

## What you will find on the website

| Section | Content |
| --- | --- |
| **Home** | Biography, research interests, profile photo, and academic links. |
| **CV** | An on-page CV preview and a link to the original PDF. |
| **Publications** | A publication list and a link to the ORCID record. |
| **Repositories** | Selected public GitHub repositories and R packages. |
| **Current Projects** | Three research projects with images, descriptions, and mathematical formulations. |
| **Presentations** | Links to presentation PDFs, displayed when files are added to the corresponding folder. |
| **Teaching** | Courses taught, course pages with PDF materials, and teaching assistantships. |

## Repository guide

| Path | Purpose |
| --- | --- |
| `_config.yml` | Name, site URL, email, social links, ORCID, and general settings. |
| `_pages/` | Content for the main website sections. |
| `_courses/` | One page per course, including course details and materials. |
| `_projects/` | One page per research project. |
| `_includes/`, `_layouts/`, `_sass/` | Components, templates, and styles. |
| `assets/img/` | Profile photo, project images, and CV page previews. |
| `assets/pdf/daniel-saavedra-cv.pdf` | Downloadable CV. |
| `assets/pdf/presentations/` | Presentation PDFs; each added PDF appears under **Presentations**. |
| `assets/pdf/courses/<course_slug>/` | Course PDFs; each added PDF appears on the relevant course page. |
| `.github/workflows/deploy.yml` | Automatic build and deployment to the `gh-pages` branch. |

Publications and repositories are maintained manually in `_pages/publications.md` and `_pages/repositories.md`. They do not sync automatically with ORCID or GitHub.

## Tutorial: create your own website from this repository

You only need Git and the GitHub website; the `gh` command-line tool is optional.

### 1. Download a copy

On [this repository's page](https://github.com/dlsaavedra/dlsaavedra.github.io), select **Code → Download ZIP** and extract the archive. This gives you the files without the original site's Git history.

On GitHub, create an empty repository named **`YOUR_USERNAME.github.io`**, replacing `YOUR_USERNAME` with your GitHub username. Do not select the options to add a README, `.gitignore`, or license; the downloaded files already include them. This repository name gives you the site URL `https://YOUR_USERNAME.github.io/`.

### 2. Replace personal information

Before publishing, review these files:

1. In `_config.yml`, update `first_name`, `last_name`, `email`, `url`, `github_username`, `orcid_id`, and the other social links. For a `YOUR_USERNAME.github.io` repository, set `url: https://YOUR_USERNAME.github.io` and leave `baseurl` empty.
2. In `_pages/about.md`, replace the biography, research focus, and profile photo. The current photo is `assets/img/daniel-saavedra.jpg`.
3. In `_pages/cv.md`, update the PDF link and preview images. Replace `assets/pdf/daniel-saavedra-cv.pdf`. The current preview displays **two page images**; if your CV has a different page count, generate one image per page and adjust the `<img>` elements in `_pages/cv.md`.
4. Update `_pages/publications.md`, `_pages/repositories.md`, and `_pages/teaching.md` with your own information. Review `_courses/` and `_projects/`, which contain the current site's academic content.
5. Replace or remove personal files you do not need from `assets/img/` and `assets/pdf/`. Update this README to describe your version of the site.

### 3. Add content

- **Presentations:** place `.pdf` files in `assets/pdf/presentations/`. Filenames become link labels, so use descriptive names such as `bayesian-extremes-2026.pdf`.
- **Course materials:** each file in `_courses/` declares a `course_slug`. Place its PDFs in `assets/pdf/courses/<course_slug>/`; they will appear automatically on that course's page.
- **New courses:** create a Markdown page in `_courses/` with `layout: page`, `title`, `institution`, `period`, `order`, and `course_slug`. Add `{% raw %}{% include course_materials.html slug=page.course_slug %}{% endraw %}` where the materials list should appear.
- **New projects:** create a Markdown page in `_projects/` with `layout: page`, `title`, `description`, `img`, and `importance`. Store its image in `assets/img/projects/`. **Current Projects** sorts its cards by `importance`.
- **Navigation:** the main pages are in `_pages/`. Their `nav` and `nav_order` fields control visibility and order in the menu.

### 4. Preview locally (optional)

Install Ruby and Bundler, open a terminal in the extracted directory, and run:

```bash
bundle install
bundle exec jekyll serve
```

Open `http://localhost:4000/`. Some Jekyll plugins in this repository may require additional system dependencies. See `.github/workflows/deploy.yml` for the tools used by the published build, or consult the [official Jekyll guide](https://jekyllrb.com/docs/) for Ruby setup.

### 5. Publish on GitHub

In the extracted directory, after replacing the personal information, run:

```bash
git init
git branch -M main
git add .
git commit -m "Create my academic website"
git remote add origin https://github.com/YOUR_USERNAME/YOUR_USERNAME.github.io.git
git push -u origin main
```

The workflow in `.github/workflows/deploy.yml` builds the site after each push to `main` or `master` and writes the result to `gh-pages`. Once that branch exists, open **Settings → Pages → Build and deployment** in your GitHub repository and select **Deploy from a branch → `gh-pages` → `/(root)`**. Check **Actions** to confirm that the build completed. See the [GitHub Pages publishing guide](https://docs.github.com/en/pages/getting-started-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site) for details.

If your repository has a name other than `YOUR_USERNAME.github.io`, set `baseurl` in `_config.yml` to `/<repository-name>` and check that links and images work under that path.

## Recommendations

- Replace the CV, photo, and personal links before your first `git push`: the GitHub Pages site is publicly accessible.
- Use filenames without spaces or accented characters for PDFs and images, since filenames become part of URLs.
- Keep each course's PDFs in its own folder and review the links after adding files.
- Check **Actions** for a completed build before troubleshooting a change that has not appeared on the website.
- Keep `LICENSE` and the attribution to [al-folio](https://github.com/alshedivat/al-folio) when reusing the code. The `LICENSE` file contains its MIT license.
