// Custom JavaScript for Self dot send blog

document.addEventListener('DOMContentLoaded', function() {
  // Add navigation
  const header = document.createElement('header');
  header.innerHTML = `
    <nav>
      <a href="/">Blog</a>
      <a href="/archive.html">Archive</a>
      <a href="/tags.html">Tag</a>
      <a href="/about.html">About</a>
      <a href="https://github.com/metacritical" class="github-link">GitHub</a>
    </nav>
  `;
  document.body.insertBefore(header, document.body.firstChild);
  
  // Add profile section
  const profileSection = document.createElement('div');
  profileSection.className = 'profile';
  profileSection.innerHTML = `
    <h1>Self dot send (Computing Blog)</h1>
    <p>Message passing is just a procedure call.</p>
  `;
  document.getElementById('content').insertBefore(profileSection, document.getElementById('content').firstChild);
  
  // Add footer
  const footer = document.createElement('footer');
  footer.innerHTML = `
    <p>Copyright © 2025 <a href="https://github.com/metacritical">Pankaj Doharey</a> — Powered by <a href="https://github.com/metacritical/alchemist">Alchemist</a></p>
  `;
  document.body.appendChild(footer);
});
