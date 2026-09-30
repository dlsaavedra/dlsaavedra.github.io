# frozen_string_literal: true

require 'pathname'
require 'yaml'

module Jekyll
  class PresentationCatalog < Generator
    safe true
    priority :low

    ROOT = File.join('assets', 'pdf', 'presentations').freeze
    CATEGORIES = %w[conferences seminars invited-talks posters].freeze

    def generate(site)
      catalog = CATEGORIES.each_with_object({}) { |category, result| result[category] = [] }

      CATEGORIES.each do |category|
        category_path = File.join(site.source, ROOT, category)
        next unless Dir.exist?(category_path)

        Dir.children(category_path).sort.each do |folder_name|
          next if folder_name.start_with?('.', '_')

          entry_path = File.join(category_path, folder_name)
          next unless File.directory?(entry_path) && !File.symlink?(entry_path)

          metadata_path = File.join(entry_path, 'info.yml')
          next unless File.file?(metadata_path)

          begin
            metadata = YAML.safe_load(File.read(metadata_path, encoding: 'UTF-8'), aliases: false)
          rescue Psych::Exception => e
            Jekyll.logger.warn 'Presentations:', "Invalid info.yml in #{entry_path}: #{e.message}"
            next
          end
          unless metadata.is_a?(Hash)
            Jekyll.logger.warn 'Presentations:', "Invalid info.yml in #{entry_path}"
            next
          end

          event = metadata['event'].to_s.strip
          title = metadata['title'].to_s.strip
          pdf_name = metadata['pdf'].to_s.strip
          pdf_path = File.join(entry_path, pdf_name)

          unless !event.empty? && !title.empty? && pdf_name == File.basename(pdf_name) &&
                 pdf_name.downcase.end_with?('.pdf') && File.file?(pdf_path) && !File.symlink?(pdf_path)
            Jekyll.logger.warn 'Presentations:', "Complete event, title, and a PDF in #{entry_path}"
            next
          end

          relative_pdf_path = Pathname.new(pdf_path)
                                      .relative_path_from(Pathname.new(site.source)).to_s
          catalog.fetch(category) << {
            'event' => event,
            'title' => title,
            'pdf_url' => "/#{relative_pdf_path}",
          }
        end
      end

      site.data['presentation_catalog'] = catalog
    end
  end
end
