#!/bin/bash
# Script to fix case-sensitive filenames for Linux builds
# Run this from anywhere in the project

echo "Fixing Haskell source file names for Linux..."

# Navigate to project root (adjust path if needed)
PROJECT_ROOT=~/Documents/molrfr/reactive_functional_robotics
cd "$PROJECT_ROOT" || exit 1

echo "Working in: $PWD"
echo ""

# Function to rename file if it exists (case-insensitive check)
rename_if_needed() {
    local filepath="$1"
    local old_name="$2"
    local new_name="$3"
    
    # Check if file exists with different case
    if [ -f "$filepath/$old_name" ] && [ ! -f "$filepath/$new_name" ]; then
        echo "Renaming $filepath/$old_name -> $filepath/$new_name"
        mv "$filepath/$old_name" "$filepath/$new_name"
    elif [ -f "$filepath/$new_name" ]; then
        echo "✓ $filepath/$new_name (correct case)"
    else
        echo "⚠ Warning: $filepath/$old_name not found"
    fi
}

# Fix FsmFrontend files
echo "=== FsmFrontend ==="
if [ -d "FsmFrontend/src" ]; then
    rename_if_needed "FsmFrontend/src" "fsmfrontend.hs" "FsmFrontend.hs"
    rename_if_needed "FsmFrontend/src" "fsmfrontendlinux.hs" "FsmFrontendLinux.hs"
    rename_if_needed "FsmFrontend/src" "main.hs" "Main.hs"
else
    echo "⚠ FsmFrontend/src directory not found"
fi
echo ""

# Fix FsmBuilder files
echo "=== FsmBuilder ==="
if [ -d "FsmBuilder/src" ]; then
    rename_if_needed "FsmBuilder/src" "fsmbuilder.hs" "FsmBuilder.hs"
    rename_if_needed "FsmBuilder/src" "graphbuilder.hs" "GraphBuilder.hs"
    rename_if_needed "FsmBuilder/src" "inputbuilder.hs" "InputBuilder.hs"
    rename_if_needed "FsmBuilder/src" "outputbuilder.hs" "OutputBuilder.hs"
    rename_if_needed "FsmBuilder/src" "mixedbuilder.hs" "MixedBuilder.hs"
    rename_if_needed "FsmBuilder/src" "main.hs" "Main.hs"
else
    echo "⚠ FsmBuilder/src directory not found"
fi
echo ""

# Fix FsmRunner files
echo "=== FsmRunner ==="
if [ -d "FsmRunner/src" ]; then
    rename_if_needed "FsmRunner/src" "fsmrunner.hs" "FsmRunner.hs"
    rename_if_needed "FsmRunner/src" "main.hs" "Main.hs"
else
    echo "⚠ FsmRunner/src directory not found"
fi
echo ""

# Fix MessageCollector files
echo "=== MessageCollector ==="
if [ -d "MessageCollector/src" ]; then
    rename_if_needed "MessageCollector/src" "messagecollector.hs" "MessageCollector.hs"
    rename_if_needed "MessageCollector/src" "main.hs" "Main.hs"
else
    echo "⚠ MessageCollector/src directory not found"
fi
echo ""

# Fix RfrLibrary files
echo "=== RfrLibrary ==="
if [ -d "RfrLibrary/src" ]; then
    rename_if_needed "RfrLibrary/src" "rfrUtilities.hs" "RfrUtilities.hs"
    rename_if_needed "RfrLibrary/src" "ymlWrapper.hs" "YmlWrapper.hs"
else
    echo "⚠ RfrLibrary/src directory not found"
fi
echo ""

echo "=== Summary ==="
echo "Listing all Haskell files in src directories:"
echo ""
find . -type f -name "*.hs" -path "*/src/*" | grep -v "dist-newstyle" | sort

echo ""
echo "Done! You can now run:"
echo "  cabal build all"
echo "  cabal run FsmFrontend -- --os=linux"
