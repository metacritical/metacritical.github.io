# Metacritical Theme for Alchemist

A clean, minimal theme inspired by the "Self dot send" blog. This theme features:

- Crimson header with clean navigation
- GitHub integration
- Excellent syntax highlighting for code blocks using native Org-mode htmlize
- Mobile-responsive design
- Clean typography with good readability

## Features

### Syntax Highlighting

This theme uses Org-mode's built-in syntax highlighting capabilities through the `htmlize` package, which provides better highlighting than external JavaScript libraries. The theme includes custom CSS to style the syntax highlighting in a Monokai-inspired color scheme.

### Navigation

The theme includes a clean navigation bar with links to:
- Blog
- Archive
- Tag
- About
- GitHub profile

### Template Structure

The theme includes the following templates:
- `container.html` - Main wrapper template
- `post.html` - Blog post template
- `index.html` - Blog listing template

### CSS Structure

The theme includes two main CSS files:
- `metacritical.css` - Main theme styles
- `syntax-highlight.css` - Code syntax highlighting styles

## Usage

To use this theme with Alchemist, add this to your blog configuration:

```elisp
(setq alchemist-blog-theme "metacritical")
```

## Customization

You can customize this theme by modifying the CSS files in the `assets/css` directory. The main color scheme is defined using CSS variables at the top of the `metacritical.css` file.
