/**
 * ditaa - Diagrams Through Ascii Art
 *
 * Copyright (C) 2004-2011 Efstathios Sideris
 *
 * Licensed under the GNU Lesser General Public License, version 3 or later.
 */
package org.stathissideris.ascii2image.core;

import java.awt.image.RenderedImage;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;

import javax.imageio.ImageIO;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.stathissideris.ascii2image.graphics.BitmapRenderer;
import org.stathissideris.ascii2image.graphics.Diagram;
import org.stathissideris.ascii2image.graphics.SVGRenderer;
import org.stathissideris.ascii2image.text.TextGrid;

/**
 * Headless request server for repeated Ditaa renders.
 *
 * Protocol, one request per line:
 *   RENDER<TAB>base64(output path)<TAB>base64(UTF-8 diagram body)
 *
 * Responses are:
 *   OK
 *   ERR<TAB>base64(UTF-8 error message)
 *
 * Renderer options are supplied once as normal Ditaa options when starting
 * the server. Keeping the JVM alive avoids repeated desktop application
 * activation and lets a publish render many diagrams through one process.
 */
public final class DitaaServer {

    private DitaaServer() {}

    private static Options createOptions() {
        Options options = new Options();
        options.addOption(new Option("S", "no-shadows", false, "Turns off drop shadows."));
        options.addOption(new Option("A", "no-antialias", false, "Turns anti-aliasing off."));
        options.addOption(new Option("W", "fixed-slope", false, "Uses fixed slopes."));
        options.addOption(new Option("d", "debug", false, "Renders the debug grid."));
        options.addOption(new Option("r", "round-corners", false, "Rounds corners."));
        options.addOption(new Option("E", "no-separation", false, "Prevents edge separation."));
        options.addOption(new Option("T", "transparent", false, "Uses a transparent background."));
        options.addOption(new Option("s", "scale", true, "Rendering scale."));
        options.addOption(new Option("t", "tabs", true, "Tab width."));
        options.addOption(new Option("b", "background", true, "Background colour."));
        options.addOption(new Option("e", "encoding", true, "Input encoding."));
        options.addOption(new Option(null, "svg", false, "Render SVG output."));
        options.addOption(new Option(null, "svg-font-url", true, "SVG font URL."));
        return options;
    }

    private static CommandLine parseOptions(String[] args) throws Exception {
        CommandLineParser parser = new PosixParser();
        return parser.parse(createOptions(), args);
    }

    private static String decode(String value) {
        return new String(Base64.getDecoder().decode(value), StandardCharsets.UTF_8);
    }

    private static String encode(String value) {
        return Base64.getEncoder().encodeToString(value.getBytes(StandardCharsets.UTF_8));
    }

    private static void render(CommandLine commandLine, String outputPath, String body) throws Exception {
        ConversionOptions options = new ConversionOptions(commandLine);
        TextGrid grid = new TextGrid();
        Path input = Files.createTempFile("ditaa-server-", ".txt");
        try {
            Files.write(input, body.getBytes(StandardCharsets.UTF_8));
            if (!grid.loadFrom(input.toString(), options.processingOptions)) {
                throw new IOException("Ditaa could not read the request body");
            }

            Diagram diagram = new Diagram(grid, options);
            File output = new File(outputPath);
            File parent = output.getParentFile();
            if (parent != null) {
                parent.mkdirs();
            }

            if (options.renderingOptions.getImageType() == RenderingOptions.ImageType.SVG) {
                String svg = new SVGRenderer().renderToImage(diagram, options.renderingOptions);
                Files.write(output.toPath(), svg.getBytes(StandardCharsets.UTF_8));
            } else {
                RenderedImage image = new BitmapRenderer().renderToImage(diagram, options.renderingOptions);
                if (!ImageIO.write(image, "png", output)) {
                    throw new IOException("No PNG writer is available");
                }
            }
        } finally {
            Files.deleteIfExists(input);
        }
    }

    public static void main(String[] args) throws Exception {
        System.setProperty("java.awt.headless", "true");
        CommandLine commandLine = parseOptions(args);

        try (BufferedReader input = new BufferedReader(
                     new InputStreamReader(System.in, StandardCharsets.UTF_8));
             BufferedWriter output = new BufferedWriter(
                     new OutputStreamWriter(System.out, StandardCharsets.UTF_8))) {
            String line;
            while ((line = input.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                try {
                    String[] fields = line.split("\\t", -1);
                    if (fields.length != 3 || !"RENDER".equals(fields[0])) {
                        throw new IllegalArgumentException("Invalid Ditaa server request");
                    }
                    render(commandLine, decode(fields[1]), decode(fields[2]));
                    output.write("OK");
                } catch (Exception error) {
                    output.write("ERR\t" + encode(error.toString()));
                }
                output.newLine();
                output.flush();
            }
        }
    }
}
