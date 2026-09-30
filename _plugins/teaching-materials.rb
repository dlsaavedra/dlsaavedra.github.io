# frozen_string_literal: true

require 'digest'
require 'pathname'

module Jekyll
  class TeachingMaterialsGenerator < Generator
    safe true
    priority :low

    CATEGORIES = { 'Docencia' => 'docencia', 'Ayudantias' => 'ayudantias' }.freeze

    def generate(site)
      catalog = { 'docencia' => [], 'ayudantias' => [] }

      CATEGORIES.each do |folder_name, category|
        category_path = File.join(site.source, 'Material_Cursos', folder_name)
        next unless Dir.exist?(category_path)

        course_names = visible_entries(category_path).select do |name|
          File.directory?(File.join(category_path, name)) &&
            File.file?(File.join(category_path, name, 'descript.txt'))
        end

        course_slugs = slugs_for(course_names)
        course_names.each do |name|
          course_path = File.join(category_path, name)
          title, details = description_for(course_path, name)
          url_parts = [category, course_slugs.fetch(name)]
          course_url = page_url(url_parts)

          catalog.fetch(category) << {
            'title' => title,
            'details' => details,
            'url' => course_url,
          }

          add_folder_page(site, course_path, url_parts, title, details,
                          [{ 'name' => 'Teaching', 'url' => '/teaching/' }])
        end
        catalog.fetch(category).sort_by! { |course| course.fetch('title').downcase }
      end

      site.data['teaching_courses'] = catalog
    end

    private

    def description_for(course_path, folder_name)
      text = File.read(File.join(course_path, 'descript.txt'), encoding: 'UTF-8')
                 .scrub.delete_prefix("\uFEFF").strip
      lines = text.lines.map(&:strip).reject(&:empty?)
      title, period = lines.shift.to_s.split(/\s+--\s+/, 2)
      title = humanize(folder_name) if title.nil? || title.empty?
      [title, [period, *lines].compact.join(' ').strip]
    end

    def add_folder_page(site, folder_path, url_parts, title, description, breadcrumbs)
      entries = visible_entries(folder_path)
      folders = entries.select { |name| File.directory?(File.join(folder_path, name)) }
      files = entries.select { |name| File.file?(File.join(folder_path, name)) }
      folder_slugs = slugs_for(folders)

      child_folders = folders.map do |name|
        {
          'name' => humanize(name),
          'url' => page_url(url_parts + [folder_slugs.fetch(name)]),
        }
      end
      material_files = files.map do |name|
        path = Pathname.new(File.join(folder_path, name))
                       .relative_path_from(Pathname.new(site.source)).to_s
        { 'name' => name, 'url' => "/#{path}" }
      end

      page = PageWithoutAFile.new(site, site.source,
                                  File.join('teaching', 'material', *url_parts), 'index.html')
      page.data.merge!({
        'layout' => 'page',
        'title' => title,
        'description' => description,
        'material_breadcrumbs' => breadcrumbs,
        'material_folders' => child_folders,
        'material_files' => material_files,
      })
      page.content = '{% include teaching_material_listing.html %}'
      site.pages << page

      child_folders.each_with_index do |child, index|
        name = folders.fetch(index)
        add_folder_page(site, File.join(folder_path, name),
                        url_parts + [folder_slugs.fetch(name)], child.fetch('name'),
                        title, breadcrumbs + [{ 'name' => title, 'url' => page_url(url_parts) }])
      end
    end

    def visible_entries(folder_path)
      Dir.children(folder_path).reject do |name|
        name.start_with?('.', '_') || name.casecmp('descript.txt').zero? ||
          File.symlink?(File.join(folder_path, name))
      end.sort_by(&:downcase)
    end

    def slugs_for(names)
      used = {}
      names.each_with_object({}) do |name, slugs|
        base = Utils.slugify(name)
        base = 'material' if base.empty?
        slug = base
        slug = "#{base}-#{Digest::SHA256.hexdigest(name)[0, 8]}" if used[slug]
        used[slug] = true
        slugs[name] = slug
      end
    end

    def humanize(name)
      name.tr('_', ' ').gsub(/\s+/, ' ').strip
    end

    def page_url(parts)
      "/teaching/material/#{parts.join('/')}/"
    end
  end
end
