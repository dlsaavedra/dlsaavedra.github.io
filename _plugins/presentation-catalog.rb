# frozen_string_literal: true

require 'date'
require 'pathname'
require 'yaml'

module Jekyll
  class PresentationCatalog < Generator
    safe true
    priority :low

    ROOT = File.join('assets', 'pdf', 'presentations').freeze
    CATEGORIES = %w[conferences seminars attendance posters].freeze
    ATTACHMENT_TYPES = {
      '.pdf' => 'PDF',
      '.jpg' => 'Image',
      '.jpeg' => 'Image',
      '.png' => 'Image',
      '.webp' => 'Image',
    }.freeze

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
            metadata = YAML.safe_load(File.read(metadata_path, encoding: 'UTF-8'), permitted_classes: [Date], aliases: false)
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
          attachment_name = metadata['file'].to_s.strip
          attachment_name = metadata['pdf'].to_s.strip if attachment_name.empty?
          unless !event.empty? && !title.empty? && (category == 'attendance' || !attachment_name.empty?)
            Jekyll.logger.warn 'Presentations:', "Complete event, title, and any required attachment in #{entry_path}"
            next
          end

          entry = {
            'event' => event,
            'title' => title,
            '_sort_date' => sort_date_for(metadata['date'], folder_name, event),
          }
          unless attachment_name.empty?
            attachment_path = File.join(entry_path, attachment_name)
            attachment_type = ATTACHMENT_TYPES[File.extname(attachment_name).downcase]
            unless attachment_name == File.basename(attachment_name) && attachment_type &&
                   File.file?(attachment_path) && !File.symlink?(attachment_path)
              Jekyll.logger.warn 'Presentations:', "Invalid attachment in #{entry_path}"
              next
            end

            relative_attachment_path = Pathname.new(attachment_path)
                                               .relative_path_from(Pathname.new(site.source)).to_s
            entry['attachment_url'] = "/#{relative_attachment_path}"
            entry['attachment_label'] = attachment_type
          end
          catalog.fetch(category) << entry
        end

        catalog.fetch(category).sort_by! do |entry|
          [-entry.fetch('_sort_date'), entry.fetch('title').downcase]
        end
        catalog.fetch(category).each { |entry| entry.delete('_sort_date') }
      end

      site.data['presentation_catalog'] = catalog
    end

    private

    def sort_date_for(value, folder_name, event)
      date_text = value.to_s.strip
      unless date_text.empty?
        if date_text.match?(/\A\d{4}-\d{2}-\d{2}\z/)
          begin
            return Date.iso8601(date_text).strftime('%Y%m%d').to_i
          rescue Date::Error
            # Fall back to a year in the folder or event name.
          end
        end
        Jekyll.logger.warn 'Presentations:', "Invalid date #{date_text.inspect} in #{folder_name}; use YYYY-MM-DD"
      end

      year = folder_name[/((?:19|20)\d{2})/, 1] || event[/((?:19|20)\d{2})/, 1]
      year ? year.to_i * 10_000 : 0
    end
  end
end
