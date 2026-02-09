#!/bin/bash

# Script to migrate content from the original Self dot send blog

SOURCE_DIR="/Users/pankajdoharey/Development/selfdotsend_old"
TARGET_DIR="/Users/pankajdoharey/Development/selfdotsend-new"

# Function to show usage
show_usage() {
  echo "Usage: $0 [command]"
  echo "Commands:"
  echo "  posts    - Copy posts from original blog"
  echo "  media    - Copy media files from original blog"
  echo "  all      - Copy all content from original blog"
  echo "  help     - Show this help message"
}

# Function to copy posts
copy_posts() {
  echo "Copying posts from original blog..."
  mkdir -p "$TARGET_DIR/posts"
  if [ -d "$SOURCE_DIR/posts" ]; then
    cp -v "$SOURCE_DIR/posts/"*.org "$TARGET_DIR/posts/"
    echo "Done copying posts!"
  else
    echo "Posts directory not found in source: $SOURCE_DIR/posts"
  fi
}

# Function to copy media files
copy_media() {
  echo "Copying media files from original blog..."
  mkdir -p "$TARGET_DIR/media/img"
  mkdir -p "$TARGET_DIR/media/css"
  mkdir -p "$TARGET_DIR/media/js"
  
  # Copy media directory if it exists
  if [ -d "$SOURCE_DIR/media" ]; then
    cp -rv "$SOURCE_DIR/media/"* "$TARGET_DIR/media/"
    echo "Done copying media files!"
  else
    echo "Media directory not found in source: $SOURCE_DIR/media"
  fi
}

# Function to copy all content
copy_all() {
  copy_posts
  copy_media
  echo "All content migrated successfully!"
}

# Main logic
case "$1" in
  posts)
    copy_posts
    ;;
  media)
    copy_media
    ;;
  all)
    copy_all
    ;;
  help|"")
    show_usage
    ;;
  *)
    echo "Unknown command: $1"
    show_usage
    exit 1
    ;;
esac
